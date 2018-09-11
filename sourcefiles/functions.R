amp_heatmap <- function(data,
         group_by = NULL,
         facet_by = NULL,
         normalise_by = NULL,
         scale_by = NULL,
         tax_aggregate = "Phylum",
         tax_add = NULL,
         tax_show = 10,
         tax_class = NULL,
         tax_empty = "best",
         order_x_by = NULL,
         order_y_by = NULL,
         plot_values = TRUE,
         plot_legendbreaks = NULL,
         plot_colorscale = "log10", 
         plot_na = TRUE, 
         plot_values_size = 4,
         plot_theme = "normal",
         measure = "mean",
         min_abundance = 0.1,
         max_abundance = NULL, 
         sort_by = NULL, 
         color_vector = NULL,
         round = 1,
         raw = FALSE,
         textmap = FALSE) {
  
  ### Data must be in ampvis2 format
  if(class(data) != "ampvis2")
    stop("The provided data is not in ampvis2 format. Use amp_load() to load your data before using ampvis functions. (Or class(data) <- \"ampvis2\", if you know what you are doing.)")
  
  ## Clean up the taxonomy
  data <- amp_rename(data = data,
                     tax_class = tax_class,
                     tax_empty = tax_empty, 
                     tax_level = tax_aggregate)
  
  #tax_add and tax_aggregate can't be the same
  if(!is.null(tax_aggregate) & !is.null(tax_add)) {
    if(identical(tax_aggregate, tax_add)) {
      stop("tax_aggregate and tax_add cannot be the same")
    }
  }
  
  ## Extract the data into separate objects for readability
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  metadata <- data[["metadata"]]
  
  ## Coerce the group_by and facet_by variables to factor to always be considered categorical. Fx Year is automatically loaded as numeric by R, but it should be considered categorical. 
  ## Grouping a heatmap by a continuous variable doesn't make sense 
  if(!is.null(group_by)) {
    metadata[group_by] <- lapply(metadata[group_by], factor)
  }
  
  if(!is.null(facet_by)) {
    if(is.null(group_by)) {
      group_by <- facet_by
    }
    metadata[facet_by] <- lapply(metadata[facet_by], factor)
  }
  
  ## Scale the data by a selected metadata sample variable
  if (!is.null(scale_by)){
    variable <- as.numeric(metadata[,scale_by])
    abund <- t(t(abund)*variable)
  }
  
  if (raw == FALSE){
    abund <- as.data.frame(apply(abund, 2, function(x) x/sum(x)*100))
  }
  
  
  ## Make a name variable that can be used instead of tax_aggregate to display multiple levels 
  suppressWarnings(
    if (!is.null(tax_add)){
      if (tax_add != tax_aggregate) {
        tax <- data.frame(tax, Display = apply(tax[,c(tax_add,tax_aggregate)], 1, paste, collapse="; "))
      }
    } else {
      tax <- data.frame(tax, Display = tax[,tax_aggregate])
    }
  )  
  
  # Aggregate to a specific taxonomic level
  abund3 <- cbind.data.frame(Display = tax[,"Display"], abund) %>%
    tidyr::gather(key = Sample, value = Abundance, -Display) %>% as.data.table()
  
  abund3 <- abund3[, "sum":=sum(Abundance), by=list(Display, Sample)] %>%
    setkey(Display, Sample) %>%
    as.data.frame()
  
  ## Add group information
  
  if(!is.null(facet_by)){
    ogroup <- group_by
    group_by <- c(group_by, facet_by)
  }
  
  suppressWarnings(
    if (!is.null(group_by)){
      if (length(group_by) > 1){
        grp <- data.frame(Sample = metadata[,1], Group = apply(metadata[,group_by], 1, paste, collapse = " ")) 
        oldGroup <- unique(cbind.data.frame(metadata[,group_by], Group = grp$Group))
      } else{
        grp <- data.frame(Sample = metadata[,1], Group = metadata[,group_by]) 
      }
      abund3$Group <- grp$Group[match(abund3$Sample, grp$Sample)]
      abund5 <- abund3
    } else{ abund5 <- data.frame(abund3, Group = abund3$Sample)}
  )
  
  ## Take the average to group level
  
  if (measure == "mean"){
    abund6 <- data.table(abund5)[, Abundance:=mean(sum), by=list(Display, Group)] %>%
      setkey(Display, Group) %>%
      unique() %>% 
      as.data.frame()
  }
  
  if (measure == "max"){
    abund6 <- data.table(abund5)[, Abundance:=max(sum), by=list(Display, Group)] %>%
      setkey(Display, Group) %>%
      unique() %>% 
      as.data.frame()
  }  
  
  if (measure == "median"){
    abund6 <- data.table(abund5)[, Abundance:=median(sum), by=list(Display, Group)] %>%
      setkey(Display, Group) %>%
      unique() %>% 
      as.data.frame()
  }
  
  
  ## Find the X most abundant levels
  if (measure == "mean"){
    TotalCounts <- group_by(abund6, Display) %>%
      summarise(Abundance = sum(Abundance)) %>%
      arrange(desc(Abundance))
  }
  
  if (measure == "max"){
    TotalCounts <- group_by(abund6, Display) %>%
      summarise(Abundance = max(Abundance)) %>%
      arrange(desc(Abundance))
  }
  
  if (measure == "median"){
    TotalCounts <- group_by(abund6, Display) %>%
      summarise(Abundance = median(Abundance)) %>%
      arrange(desc(Abundance))
  }
  
  ## Subset to X most abundant levels
  if (is.numeric(tax_show)){
    if (tax_show > nrow(TotalCounts)){  
      tax_show <- nrow(TotalCounts)
    }
    abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax_show])
  }
  
  ## Subset to a list of level names
  if (!is.numeric(tax_show)){
    if (tax_show != "all"){
      abund7 <- filter(abund6, Display %in% tax_show)    
    }
    ### Or just show all  
    if (tax_show == "all"){
      tax_show <- nrow(TotalCounts)  
      abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax_show]) 
    }
  }
  abund7 <- as.data.frame(abund7)
  
  ## Normalise to a specific group (The Abundance of the group is set as 1)  
  if(!is.null(normalise_by)){
    #temp <- dcast(abunds7, Display~Group, value.var = "Abundance") # Is this working?
    temp <- tidyr::spread(abund7, key = Group, value = Abundance) 
    temp1 <- cbind.data.frame(Display = temp$Display, temp[,-1]/temp[,normalise_by])   
    abund7 <- tidyr::gather(temp1, key = Group, value = Abundance, -Display)
  } 
  
  ## Order.y
  if (is.null(order_y_by)){
    abund7$Display <- factor(abund7$Display, levels = rev(TotalCounts$Display))
  }
  if (!is.null(order_y_by)){
    if ((length(order_y_by) == 1) && (order_y_by != "cluster")){
      temp1 <- filter(abund7, Group == order_y_by) %>%
        group_by(Display) %>%
        summarise(Mean = mean(Abundance)) %>%
        arrange(desc(Mean))
      
      abund7$Display <- factor(abund7$Display, levels = rev(temp1$Display))
    }
    if (length(order_y_by) > 1){
      abund7$Display <- factor(abund7$Display, levels = order_y_by)
    }
    if ((length(order_y_by) == 1) && (order_y_by == "cluster")){
      if (is.null(max_abundance)){max_abundance <- max(abund7$Abundance)}
      tdata <- mutate(abund7, 
                      Abundance = ifelse(Abundance < min_abundance, min_abundance, Abundance),
                      Abundance = ifelse(Abundance > max_abundance, max_abundance, Abundance))
      tdata <- dcast(tdata, Display~Group, value.var = "Abundance") #is this working?
      #tdata <- tidyr::spread(tdata, key = Group, value = Abundance) 
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[,-1]
      tclust <- hclust(dist(tdata2))
      tnames <- levels(droplevels(tdata$Display))[tclust$order]
      abund7$Display <- factor(abund7$Display, levels = tnames)
    }
  }
  
  ## Order.x
  if (!is.null(order_x_by)){
    if ((length(order_x_by) == 1) && (order_x_by != "cluster")){
      temp1 <- filter(abund7, Display == order_x_by) %>%
        group_by(Group) %>%
        summarise(Mean = mean(Abundance)) %>%
        arrange(desc(Mean))
      abund7$Group <- factor(abund7$Group, levels = as.character(temp1$Group))
    }    
    if (length(order_x_by) > 1){
      abund7$Group <- factor(abund7$Group, levels = order_x_by)
    }
    if ((length(order_x_by) == 1) && (order_x_by == "cluster")){
      if (is.null(max_abundance)){max_abundance <- max(abund7$Abundance)}
      tdata <- mutate(abund7, 
                      Abundance = ifelse(Abundance < min_abundance, min_abundance, Abundance),
                      Abundance = ifelse(Abundance > max_abundance, max_abundance, Abundance))
      tdata <- dcast(tdata, Display~Group, value.var = "Abundance") #is this working?
      #tdata <- tidyr::spread(tdata, key = Group, value = Abundance) 
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[,-1]
      tclust <- hclust(dist(t(tdata2)))
      tnames <- tclust$labels[tclust$order]
      abund7$Group <- factor(abund7$Group, levels = tnames) 
    }
  }
  
  ## Handle NA values
  if(plot_na == FALSE){ plot_na <- "grey50" }else{ if(!is.null(color_vector)) {plot_na <-color_vector[1]} else {plot_na <-"#67A9CF"}}  
  
  ## Scale to percentages if not normalised and scaled
  
  if (length(group_by) > 1 ){ abund7 <- merge(abund7, oldGroup)}
  
  if (is.null(min_abundance)){
    min_abundance <- ifelse(min(abund7$Abundance) > 0.001, min(abund7$Abundance), 0.001)
  }
  if (is.null(max_abundance)){
    max_abundance <- max(abund7$Abundance)
  }
  
  ## Define the output 
  if (!textmap) {
    ## Make a heatmap style plot
    p <- ggplot(abund7, aes_string(x = "Group", y = "Display", label = formatC("Abundance", format = "f", digits = 1))) +     
      geom_tile(aes(fill = Abundance), colour = "white", size = 0.5) +
      theme(axis.text.y = element_text(size = 12, color = "black", vjust = 0.4),
            axis.text.x = element_text(size = 10, color = "black", vjust = 0.5, angle = 90, hjust = 1),
            axis.title = element_blank(),
            text = element_text(size = 8, color = "black"),
            #axis.ticks.length = unit(1, "mm"),
            plot.margin = unit(c(1,1,1,1), "mm"),
            title = element_text(size = 8),
            panel.background = element_blank())
    
    ## Get colorpalette for colorscale or set default
    if (!is.null(color_vector)){
      color.pal = color_vector
    } else {
      color.pal = rev(brewer.pal(3, "RdBu"))
    }
    
    if (plot_values == TRUE){
      abund8 <- abund7
      abund8$Abundance <- round(abund8$Abundance, round)
      p <- p + geom_text(data = abund8, size = plot_values_size, colour = "grey10", check_overlap = TRUE) +
        theme(legend.position = "none")
    }
    if (is.null(plot_legendbreaks)){
      p <- p +scale_fill_gradientn(colours = color.pal, trans = plot_colorscale, na.value=plot_na, oob = squish, limits = c(min_abundance, max_abundance))
    }
    if (!is.null(plot_legendbreaks)){
      p <- p +scale_fill_gradientn(colours = color.pal, trans = plot_colorscale, breaks=plot_legendbreaks, na.value=plot_na , oob = squish, limits = c(min_abundance, max_abundance))
    }
    
    
    if (is.null(normalise_by)){
      p <- p + labs(x = "", y = "", fill = "% Read\nAbundance")  
    }
    if (!is.null(normalise_by)){
      p <- p + labs(x = "", y = "", fill = "Relative")  
    }
    
    if(!is.null(facet_by)){
      if(length(ogroup) > 1){
        p$data$Group <- apply(p$data[,ogroup], 1, paste, collapse = " ")  
      } else{
        p$data$Group <- p$data[,ogroup]
      }
      
      if(plot_values == TRUE){
        if(length(ogroup) > 1){
          p$layers[[2]]$data$Group <- apply(p$layers[[2]]$data[,ogroup], 1, paste, collapse = " ")  
        } else{
          p$layers[[2]]$data$Group <- p$layers[[2]]$data[,ogroup]
        }
      }
      p <- p + facet_grid(reformulate(facet_by), scales = "free_x", space = "free")
      p <- p + theme(strip.text = element_text(size = 10))
    }
    return(p)
  } else if (textmap) {
    #raw text heatmap data frame
    textmap <- abund7[,c("Display", "Abundance", "Group")] %>% 
      group_by(Group) %>%
      filter(!duplicated(Abundance, Group)) %>%
      spread(Group, Abundance) %>% 
      arrange(desc(droplevels(Display)))
    textmap <- data.frame(textmap[,-1], row.names = textmap$Display, check.names = FALSE)
    return(textmap)
  }
}

amp_timeseries <- function(data,
                           time_variable = NULL,
                           group_by = NULL,
                           tax_aggregate = "OTU",
                           tax_add = NULL,
                           tax_show = 5,
                           tax_class = NULL,
                           tax_empty = "best",
                           split = FALSE,
                           raw = FALSE,
                           plotly = FALSE,
                           ...) {
  
  ## Clean up the taxonomy
  data <- amp_rename(data = data,
                     tax_class = tax_class,
                     tax_empty = tax_empty, 
                     tax_level = tax_aggregate)
  
  #tax_add and tax_aggregate can't be the same
  if(!is.null(tax_aggregate) & !is.null(tax_add)) {
    if(tax_aggregate == tax_add) {
      stop("tax_aggregate and tax_add cannot be the same")
    }
  }
  
  ## Extract the data into separate objects for readability
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  metadata <- data[["metadata"]]
  
  ## try to find a date column
  if(is.null(time_variable)) {
    dateCols <- unlist(lapply(metadata, lubridate::is.Date))
    if(sum(dateCols) == 1) {
      time_variable <- colnames(metadata)[which(dateCols)]
      message("No \"time_variable\" provided, assuming the column \"", time_variable, "\" contains the dates.\n")
    } else {
      stop("Please provide a valid date column by the argument time_variable.")
    }
  }
  
  ## Coerce the group_by and facet_by variables to factor to always be considered categorical. Fx Year is automatically loaded as numeric by R, but it should be considered categorical. 
  if(!is.null(group_by)) {
    metadata[group_by] <- lapply(metadata[group_by], factor)
  }
  
  if (raw == FALSE){
    abund <- as.data.frame(sapply(abund, function(x) x/sum(x)*100))
  }
  
  ## Make a name variable that can be used instead of tax_aggregate to display multiple levels 
  suppressWarnings(
    if (!is.null(tax_add)){
      if (tax_add != tax_aggregate) {
        tax <- data.frame(tax, Display = apply(tax[,c(tax_add,tax_aggregate)], 1, paste, collapse="; "))
      }
    } else {
      tax <- data.frame(tax, Display = tax[,tax_aggregate])
    }
  )  
  
  # Aggregate to a specific taxonomic level
  abund3 <- cbind.data.frame(Display = tax[,"Display"], abund) %>%
    tidyr::gather(key = Sample, value = Abundance, -Display) %>% as.data.table()
  
  abund3 <- abund3[, "sum":=sum(Abundance), by=list(Display, Sample)] %>%
    setkey(Display, Sample) %>%
    as.data.frame()
  
  suppressWarnings(
    if (!is.null(group_by)){
      if (length(group_by) > 1){
        grp <- data.frame(Sample = metadata[,1], Group = apply(metadata[,group_by], 1, paste, collapse = "; ")) 
        oldGroup <- unique(cbind.data.frame(metadata[,group_by], Group = grp$Group))
      } else{
        grp <- data.frame(Sample = metadata[,1], Group = metadata[,group_by]) 
      }
      abund3$Group <- grp$Group[match(abund3$Sample, grp$Sample)]
      abund5 <- abund3
    } else {
      abund5 <- data.frame(abund3, Group = abund3$Sample)
    }
  )
  
  TotalCounts <- group_by(abund5, Display) %>%
    summarise(Median = median(sum), Total = sum(sum), Mean = mean(sum)) %>% 
    arrange(desc(Mean))
  
  ## Subset to the x most abundant levels
  if (is.numeric(tax_show)){
    if (tax_show > nrow(TotalCounts)){  
      tax_show <- nrow(TotalCounts)
    }
    abund5$Display <- as.character(abund5$Display)
    abund7 <- subset(abund5, Display %in% as.character(unlist(TotalCounts[1:tax_show,"Display"])))
  }
  ## Subset to a list of level names
  if (!is.numeric(tax_show)){
    if (length(tax_show) > 1){
      abund7 <- subset(abund5, Display %in% tax_show)
    }
    if ((length(tax_show) == 1) && (tax_show != "all")){
      abund7 <- subset(abund5, Display %in% tax_show)
    }
    ### Or just show all  
    if ((length(tax_show) == 1) && (tax_show == "all")){
      tax_show <- nrow(TotalCounts)  
      abund7 <- subset(abund5, Display %in% as.character(unlist(TotalCounts[1:tax_show,"Display"])))  
    }
  }
  abund7 <- as.data.frame(abund7)
  abund7$Display <- factor(abund7$Display, levels = TotalCounts$Display)
  
  if (length(group_by) > 1) {
    abund7 <- merge(abund7, oldGroup)
  }
  
  abund7 <- merge(abund7, metadata, by.x = "Sample", by.y = colnames(metadata)[1])
  abund7[,time_variable] <- lubridate::as_date(abund7[,time_variable], ...)
  abund7$DisplayGroup <- paste(abund7$Display, abund7$Group)
  colnames(abund7)[which(colnames(abund7) == "Display")] <- tax_aggregate
  colnames(abund7)[which(colnames(abund7) == "sum")] <- "Value"
  abund7 <- abund7[,-c(which(colnames(abund7) == "Abundance"))]
  abund7 <- unique(abund7)
  
  if(is.null(group_by)) {
    if(any(duplicated(metadata[,time_variable]))) {
      warning("Duplicate dates in column ", time_variable, ", displaying the average for each date.\n Consider grouping dates using the group_by argument or subset the data using amp_subset_samples.\n")
      abund7 %>% 
        dplyr::group_by_(time_variable, tax_aggregate) %>% 
        dplyr::summarise_at("Value", mean, na.rm = TRUE) -> abund7
    }
    if(isTRUE(split)) {
      p <- ggplot(abund7, aes_string(x=time_variable, y="Value", group = tax_aggregate))
    } else if(!isTRUE(split)) {
      p <- ggplot(abund7, aes_string(x=time_variable, y="Value", group = tax_aggregate, color = tax_aggregate))
    }
  } else if(!is.null(group_by)) {
    if(isTRUE(split)) {
      p <- ggplot(abund7, aes_string(x=time_variable, y="Value", color = "Group", group = "DisplayGroup"))
    } else if(!isTRUE(split)) {
      p <- ggplot(abund7, aes_string(x=time_variable, y="Value", color = "Group", group = "DisplayGroup", linetype = tax_aggregate, shape = tax_aggregate))
    }
  }
  p <- p +
    geom_line() +
    geom_point() +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, vjust = 0.3, angle = 90),
          panel.grid.major.x = element_line(color = "grey90"),
          panel.grid.major.y = element_line(color = "grey90"))
  
  p <- p + ylab("Hyppighed (%)")
  
  if(split == T){
    p <- p + facet_wrap(tax_aggregate) +
      theme(strip.background = element_rect(colour=NA, fill="grey95"),
            panel.grid.major.x = element_line(color = "grey90"),
            panel.grid.major.y = element_line(color = "grey90"),
            legend.position = "bottom",
            strip.text = element_text(size = 10),
            legend.title=element_blank())
  }
  
  if(isTRUE(plotly)) {
    return(ggplotly(p, tooltip = c("Group", tax_aggregate, "Date", "Value")))
  } else if(!isTRUE(plotly)) {
    return(p)
  }
}
