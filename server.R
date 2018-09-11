#download from dropbox function
ksa_drop_download <- function(path,
                              local_path = NULL,
                              overwrite = FALSE,
                              progress = interactive(),
                              verbose = interactive(),
                              dtoken = get_dropbox_token()
) {
  if (!grepl("^(id|rev):", path)) 
    path <- rdrop2:::add_slashes(path)
  if (is.null(local_path)) {
    local_path = basename(path)
  } else if (dir.exists(local_path)) {
    local_path <- file.path(local_path, basename(path))
  }
  url <- "https://content.dropboxapi.com/2/files/download"
  req <- httr::POST(url = url,
                    httr::config(token = rdrop2:::get_dropbox_token()), 
                    httr::add_headers(`Dropbox-API-Arg` = jsonlite::toJSON(list(path = path), 
                                                                           auto_unbox = TRUE)), 
                    if (progress) 
                      httr::progress(), 
                    httr::write_disk(local_path, overwrite)
  )
  httr::stop_for_status(req)
  if (verbose) {
    size <- file.size(local_path)
    class(size) <- "object_size"
    message(sprintf("Downloaded %s to %s: %s on disk", path, 
                    local_path, format(size, units = "auto")))
  }
  invisible(paste0(req$content))
}

#get plant info
path <- drop_search("plantinfo.xls", path = "/MiDAS\ app")[["matches"]][[1]][["metadata"]][["path_lower"]]
DLstatus <- ksa_drop_download(path, local_path = paste("tempfile", tools::file_ext(path), sep = "."), overwrite = TRUE, verbose = TRUE, progress = FALSE)
plantinfo <- readxl::read_excel(DLstatus) %>% arrange(Anlæg)

shinyServer(function(input, output, session) {
  ######################### DATA #########################
  load("data/egaamarselisborg(CP332).rda")
  load("data/MiF.rda")
  load("data/MiDAS2.rda")
  load("data/MiDAS_PeriodAvg.rda")
  load("data/digester_bacteria.rda")
  load("data/digester_bacteria_PeriodAvg.rda")
  load("data/digester_archaea.rda")
  load("data/digester_archaea_PeriodAvg.rda")
  load("data/AalborgEW.rda")
  load("data/AalborgEW_PeriodAvg.rda")
  
  data <- reactive({
    if(input$dataset == "Aktivt slam") {
      as_plants <<- input$wwtp_as
      d <- MiDAS
      if(!is.null(input$wwtp_as)) {
        ds <- suppressMessages(amp_subset_samples(d, Plant %in% as_plants))
      } else
        ds <- d
    } else if(input$dataset == "Rådnetank (bakterier)") {
      db_plants <<- input$wwtp_db
      d <- digester_bacteria
      if(!is.null(input$wwtp_db)) {
        ds <- suppressMessages(amp_subset_samples(d, Plant %in% db_plants))
      } else
        ds <- d
    } else if(input$dataset == "Rådnetank (archaea)") {
      da_plants <<- input$wwtp_da
      d <- digester_archaea
      if(!is.null(input$wwtp_da)) {
        ds <- suppressMessages(amp_subset_samples(d, Plant %in% da_plants))
      } else
        ds <- d
    } else if(input$dataset == "Aalborg BioBANK (2012-2015)") {
      aaew_plants <<- input$wwtp_aalborgew
      d <- AalborgEW
      if(!is.null(input$wwtp_aalborgew)) {
        ds <- suppressMessages(amp_subset_samples(d, Plant %in% aaew_plants))
      } else
        ds <- d
    } else if(input$dataset == "CP332 (Egå+Marselisborg)") {
      CP332 <<- input$wwtp_CP332
      d <- egaamarselisborg
      if(!is.null(input$wwtp_CP332)) {
        ds <- suppressMessages(amp_subset_samples(d, Plant %in% CP332))
      } else
        ds <- d
    }
    d$metadata <- d$metadata %>% 
      mutate("Alle_anlæg" = "Alle i MiDAS")
    return(list("complete" = amp_rename(d), "subset" = ds))
  })
  
  ######################### LOGIN #########################
  auth <- reactiveValues(status = FALSE)
  observeEvent(input$login_button, {
    if((input$user_name == "MiDAS2017" & input$password == "spildevand") |
       (input$user_name == "aarhusvand" & input$password == "egaamarselisborg")) {
      auth$status <- TRUE
    } else {
      auth$status <- FALSE
    }
  })
  
  output$logintext <- renderUI({
    if(isTRUE(auth$status)) {
      shiny::tagList(
        wellPanel(
          h3("Velkommen til MiDAS dashboard"),
          tags$hr(),
          p("Dette er en online ", a(href = "https://shiny.rstudio.com/", "Shiny app"), " lavet til at give et indblik i mikrobiologien i danske renseanlæg siden år 2006. "),
          p("Al data i denne app er baseret på prøver fra danske renseanlæg, der er indsamlet og behandlet af Aalborg Universitet som en del af ", a(href="http://midasfieldguide.org", target = "_blank", "MiDAS"), " (Microbial Database of Activated Sludge) projektet, som har til formål at samle viden om de mikroorganismer, der spiller en central rolle i de biologiske processer i spildevandsrensning."),
          p("Start med at vælg et datasæt og ét eller flere anlæg til venstre. For en detaljeret guide til appen og dens funktioner se", downloadLink("guide_pdf", "denne PDF"), "."),
          br(),
          a(href="http://midasfieldguide.org", target = "_blank", img(src = "logobig.png"))
        )
      )
    }
  })
  
  ######################### SIDEBAR #########################
  output$sidebarUI <- renderMenu({
    sidebarMenu(
      menuItem(
        "Login",
        tabName = "login",
        icon = icon("user")
      ),
      if(isTRUE(auth$status)) {
        list(
          menuItem(
            "Anlæg info",
            tabName = "plantinfo",
            icon = icon("info")
          ),
          menuItem(
            text = "Bakteriesammensætning",
            tabName = "community",
            icon = icon("bug")
          ),
          menuItem(
            text = "Tidsserier",
            tabName = "timeseries",
            icon = icon("line-chart"),
            startExpanded = FALSE,
            menuSubItem(
              text = "Mest hyppige bakterier (Genus)",
              tabName = "timeseries_genus"
            ),
            menuSubItem(
              text = "Genus funktion",
              tabName = "timeseries_function"
            )
          ),
          menuItem(
            text = "Ordinering",
            tabName = "ordination",
            icon = icon("sitemap")
          ),
          tags$hr(),
          selectInput(
            inputId = "dataset",
            label = "Datasæt",
            choices = if(input$user_name == "aarhusvand") c("Aktivt slam", "Rådnetank (bakterier)", "Rådnetank (archaea)", "Aalborg BioBANK (2012-2015)", "CP332 (Egå+Marselisborg)") else c("Aktivt slam", "Rådnetank (bakterier)", "Rådnetank (archaea)", "Aalborg BioBANK (2012-2015)"),
            selected = "Aktivt slam",
            multiple = FALSE
          ),
          conditionalPanel(
            condition = "input.dataset == 'Aktivt slam'",
            selectizeInput(
              inputId = "wwtp_as",
              label = "Renseanlæg (blank for alle)",
              choices = sort(as.character(unique(MiDAS$metadata$Plant))),
              selected = c("Aalborg West", "Aalborg East"),
              multiple = TRUE,
              options = list(placeholder = "Alle")
            )
          ),
          conditionalPanel(
            condition = "input.dataset == 'Rådnetank (bakterier)'",
            selectizeInput(
              inputId = "wwtp_db",
              label = "Renseanlæg (blank for alle)",
              choices = sort(as.character(unique(digester_bacteria$metadata$Plant))),
              selected = c("Aalborg West", "Aalborg East"),
              multiple = TRUE,
              options = list(placeholder = "Alle")
            )
          ),
          conditionalPanel(
            condition = "input.dataset == 'Rådnetank (archaea)'",
            selectizeInput(
              inputId = "wwtp_da",
              label = "Renseanlæg (blank for alle)",
              choices = sort(as.character(unique(digester_archaea$metadata$Plant))),
              selected = c("Aalborg_West", "Aalborg_East"),
              multiple = TRUE,
              options = list(placeholder = "Alle")
            )
          ),
          conditionalPanel(
            condition = "input.dataset == 'Aalborg BioBANK (2012-2015)'",
            selectizeInput(
              inputId = "wwtp_aalborgew",
              label = "Renseanlæg (blank for alle)",
              choices = sort(as.character(unique(AalborgEW$metadata$Plant))),
              selected = c("Aalborg West", "Aalborg East"),
              multiple = TRUE,
              options = list(placeholder = "Alle")
            )
          ),
          conditionalPanel(
            condition = "input.dataset == 'CP332 (Egå+Marselisborg)'",
            selectizeInput(
              inputId = "wwtp_CP332",
              label = "Renseanlæg (blank for alle)",
              choices = sort(as.character(unique(egaamarselisborg$metadata$Plant))),
              selected = c("Aalborg West", "Aalborg East"),
              multiple = TRUE,
              options = list(placeholder = "Alle")
            )
          ),
          tags$hr(),
          conditionalPanel(
            condition="$('html').hasClass('shiny-busy')", 
            div(style="text-align:center",
                icon("refresh"), 
                "Loading...")
          )        
        )
      }
    )
  })
  
  ######################### PLANTINFO #########################
  output$plantinfo <- DT::renderDataTable(
    plantinfo,
    rownames = FALSE,
    options = list(
      pageLength = 50
    )
  )
  ######################### HEATMAP #########################
  output$heatmap_sort <- renderUI(
    selectizeInput(
      inputId = "heatmap_sort",
      label = "Sortér",
      choices = c("Gennemsnit af valgte", unique(data()[["subset"]]$metadata[,input$heatmap_group_by])),
      multiple = FALSE,
      width = "200px"
    )
  )
  
  output$heatmap_filtergenera <- renderUI({
    selectizeInput(
      inputId = "heatmap_filtergenera",
      label = "Vis kun specifikke bakterier (ud af valgte funktion ovenfor)",
      choices = str_remove_all(unique(as.character(heatmap_data()[["tax"]][["Genus"]])), "^[k|p|c|o|f|g|s]_*"),
      selected = "",
      multiple = TRUE,
      width = "200px",
      options = list(placeholder = "Alle")
    )
  })
  
  heatmap_data <- reactive({
    ds <- data()[["subset"]]
    if(input$genusfunction != "all") {
      ds$tax <- filter(ds$tax, grepl(paste0(as.character(MiF$Genus[which(MiF[,input$genusfunction] %in% c("POS", "VAR"))]), "$", collapse = "|"), Genus))
      rownames(ds$tax) <- ds$tax$OTU
      ds$abund <- ds$abund[rownames(ds$tax),]
    }
    return(ds)
  })
  
  heatmap <- eventReactive(input$render_heatmap, {
    #heatmap
    facet_by <- if(!input$heatmap_facet_by == "none") {input$heatmap_facet_by}
    if(input$heatmap_sort == "Gennemsnit af valgte") {
      order_by <- NULL
    } else {
      order_by <- input$heatmap_sort
    }
    ds <- heatmap_data()
    if(!is.null(input$heatmap_filtergenera)) {
      ds$tax <- filter(ds$tax, grepl(paste0(as.character(input$heatmap_filtergenera), "$", collapse = "|"), Genus))
      rownames(ds$tax) <- ds$tax$OTU
      ds$abund <- ds$abund[rownames(ds$tax),]
    }
    
    heatmap <- amp_heatmap(data = ds,
                group_by = input$heatmap_group_by, 
                facet_by = facet_by,
                tax_aggregate = "Genus",
                order_y_by = order_by,
                tax_show = input$heatmap_tax_show,
                plot_values = input$heatmap_plot_values,
                plot_colorscale = "log10",
                raw = TRUE,
                color_vector = unlist(strsplit(input$heatmap_colorvector, ","))
    )
    heatmap <- heatmap + 
      theme(legend.position = "none",
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.line = element_blank(),
            strip.text = element_text(angle = ifelse(isTRUE(input$heatmap_turnfacettext), 90, 0))
            )
    #allheatmap
    if(isTRUE(input$heatmap_showmean)) {
      d <- data()[["complete"]]
      pattern <- paste0(unique(heatmap$data$Display), "$", collapse = "|")
      pattern <- gsub("\\(", "\\\\(", pattern)
      pattern <- gsub("\\)", "\\\\)", pattern)
      d$tax <- filter(d$tax, grepl(pattern, Genus))
      rownames(d$tax) <- d$tax$OTU
      d$abund <- d$abund[rownames(d$tax),]
      allheatmap <- amp_heatmap(data = d,
                                group_by = "Alle_anlæg",
                                tax_aggregate = "Genus",
                                order_y_by = levels(droplevels(heatmap$data$Display)),
                                tax_show = input$heatmap_tax_show,
                                plot_values = input$heatmap_plot_values,
                                plot_colorscale = "log10",
                                raw = TRUE,
                                color_vector = unlist(strsplit(input$heatmap_colorvector, ","))
                                ) + 
        theme(legend.position = "none",
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.line = element_blank()
              )
    }
    
    #functions
    if(isTRUE(input$heatmap_function)) {
      function_data <- MiF
      plotted_functions = c("FIL",
                            "AOB",
                            "NOB",
                            "PAO",
                            "GAO",
                            "DN",
                            "FER",
                            "ACE",
                            "MET")
      
      # Retrieve the genus names from the plot
      names <- data.frame(do.call('rbind', strsplit(levels(droplevels(heatmap$data$Display)),'; ',fixed=TRUE)))
      names <- data.frame(Genus = names[,1])
      names$Genus <- as.character(names$Genus)
      
      # Define some nice colors for plotting
      POS <- "#31a354"
      VAR <- "orange"
      NEG <- "#f03b20"
      NT <- "grey90"
      
      # Make selection if not plotting all functions
      function_data <- function_data[,c("Genus",plotted_functions)]
      
      # Merge the genus and function information
      nameFunc <- merge(x = names, y = function_data, all.x = TRUE, all.y = FALSE) 
      nameFunc[is.na(nameFunc)] <- "NT"
      nameFuncM <- melt(nameFunc, id.vars = "Genus", value.name = "Value", variable.name = "Function")
      nameFuncM$Value <- factor(nameFuncM$Value, levels = c("POS", "VAR", "NEG", "NT"))
      nameFuncM$Genus <- factor(nameFuncM$Genus, levels = names$Genus)
      
      # Generate the plot
      functions <- ggplot(nameFuncM, aes(x = Function, y = Genus, color = Value)) +
        geom_point(size = 5) +
        scale_color_manual(values = c(POS, VAR, NEG, NT), labels = c("Positiv", "Variabel", "Negativ", "Ikke testet"), drop = FALSE) +
        theme(axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1, vjust = 0.4),
              axis.text.y = element_blank(),
              axis.title = element_blank(),
              legend.title = element_blank(), 
              axis.ticks.length = unit(1, "mm"),
              axis.ticks = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = "grey95"),
              legend.key = element_blank(),
              axis.line = element_blank()
        )
    }
    if(!isTRUE(input$heatmap_showmean) & !isTRUE(input$heatmap_function)) {
      return(heatmap)
    } else if(isTRUE(input$heatmap_showmean) & !isTRUE(input$heatmap_function)) {
      return(cowplot::plot_grid(heatmap, allheatmap, ncol = 2, rel_widths = c(0.90, 0.10), align = "h", axis = "tb"))
    } else if(isTRUE(!input$heatmap_showmean) & isTRUE(input$heatmap_function)) {
      return(cowplot::plot_grid(heatmap, functions, ncol = 2, rel_widths = c(0.70, 0.30), align = "h", axis = "tb"))
    } else if(isTRUE(input$heatmap_showmean) & isTRUE(input$heatmap_function)) {
      return(cowplot::plot_grid(heatmap, allheatmap, functions, ncol = 3, rel_widths = c(0.65, 0.10, 0.25), align = "h", axis = "tb"))
    }
  })
  
  output$heatmap <- renderPlot({
    heatmap()
  })
  
  ######################### TIMESERIES #########################
  timeseries <- eventReactive(input$render_timeseries, {
    if(input$timeseries_group == "avg") {
      group_by <- NULL
    } else if(!input$timeseries_group == "avg") {
      group_by <- "Plant"
    }
    plot <- suppressWarnings(amp_timeseries(data = data()[["subset"]],
                                            time_variable = "Date",
                                            tax_aggregate = "Genus",
                                            group_by = group_by,
                                            raw = TRUE,
                                            tax_show = input$timeseries_tax_show,
                                            split = input$timeseries_split
    ))
    plot <- plot + scale_x_date(date_labels = "%Y-%b-%d")
    plotlyplot <- ggplotly(plot) %>% 
      layout(margin = list(l = 100,
                           r = 50,
                           b = 100,
                           t = 20,
                           pad = 4
      )
      )
    plotlyplot$elementId <- NULL #To suppress warning spam
    return(plotlyplot)
  })
  
  output$timeseries <- renderPlotly({
    timeseries()
    })
  
  output$timeseries_svi <- renderPlotly({
    if(input$dataset == "Aktivt slam") {
      plot <- ggplot(data()[["subset"]]$metadata, aes(x=Date, y=SVI)) +
        geom_line(aes(group = Plant, color = Plant)) +
        geom_point(aes(group = Plant, color = Plant)) +
        xlab("") +
        ylab("Slam Volumen Index") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 10, vjust = 0.3, angle = 90),
              panel.grid.major.x = element_line(color = "grey90"),
              panel.grid.major.y = element_line(color = "grey90"),
              legend.title=element_blank()
        )
      if(input$timeseries_function_avg) {
        plot <- plot + 
          geom_line(data = MiDAS_PeriodAvg, aes(x=PeriodAvg, y=SVI), color = "black", linetype = "dotted") +
          geom_point(data = MiDAS_PeriodAvg, aes(x=PeriodAvg, y=SVI), color = "black")
      }
      
      plotlyplot <- ggplotly(plot) %>% 
        layout(margin = list(l = 70,
                             r = 50,
                             b = 50,
                             t = 20,
                             pad = 4
        )
        )
      plotlyplot$elementId <- NULL #To suppress warning spam
      return(plotlyplot)
    }
  })
  
  output$timeseries_dsvi <- renderPlotly({
    if(any(input$dataset %in% c("Aktivt slam", "Aalborg BioBANK (2012-2015)"))) {
      plot <- ggplot(data()[["subset"]]$metadata, aes(x=Date, y=DSVI)) +
        geom_line(aes(group = Plant, color = Plant)) +
        geom_point(aes(group = Plant, color = Plant)) +
        xlab("") +
        ylab("Fortyndet Slam Volumen Index") +
        theme_classic() +
        theme(axis.text.x = element_text(size = 10, vjust = 0.3, angle = 90),
              panel.grid.major.x = element_line(color = "grey90"),
              panel.grid.major.y = element_line(color = "grey90"),
              legend.title=element_blank()
        )
      if(input$timeseries_function_avg) {
        plot <- plot + 
          geom_line(data = MiDAS_PeriodAvg, aes(x=PeriodAvg, y=DSVI), color = "black", linetype = "dotted") +
          geom_point(data = MiDAS_PeriodAvg, aes(x=PeriodAvg, y=DSVI), color = "black")
      }
      
      plotlyplot <- ggplotly(plot) %>% 
        layout(margin = list(l = 70,
                             r = 50,
                             b = 50,
                             t = 20,
                             pad = 4
        )
        )
      plotlyplot$elementId <- NULL #To suppress warning spam
      return(plotlyplot)
    }
  })
  
  output$timeseries_function <- renderPlotly({
    #if(input$dataset == "Aktivt slam") {
    #  d <- data()[["subset"]]$metadata
    #} else if(any(input$dataset == c("Rådnetank (bakterier)", "Rådnetank (archaea)", "Aalborg BioBANK (2012-2015)"))) {
    #  d <- data()[["subset"]]$metadata %>% group_by(Plant, Date) %>% summarise_all(mean) %>% ungroup()
    #}
    #plot <- ggplot(d, aes_string(x="Date", y=input$timeseries_function)) +
    #  geom_line(aes(group = Plant, color = Plant)) +
    #  suppressWarnings(geom_point(aes(group = Plant, color = Plant, text = apply(d[,c("Plant", "Date", input$timeseries_function)], 1, paste, collapse = "<br>")))) +
    #  xlab("") +
    #  ylab("Hyppighed (%)") +
    #  theme_classic() +
    #  theme(axis.text.x = element_text(size = 10, vjust = 0.3, angle = 90),
    #        panel.grid.major.x = element_line(color = "grey90"),
    #        panel.grid.major.y = element_line(color = "grey90"),
    #        legend.title = element_blank()
    #  )
    #if(input$timeseries_function_avg) {
    #  if(input$dataset == "Aktivt slam") {
    #    plotdata <- MiDAS_PeriodAvg
    #  } else if(input$dataset == "Rådnetank (bakterier)") {
    #    plotdata <- digester_bacteria_PeriodAvg
    #  } else if(input$dataset == "Rådnetank (archaea)") {
    #    plotdata <- digester_archaea_PeriodAvg
    #  } else if(input$dataset == "Aalborg BioBANK (2012-2015)") {
    #    plotdata <- AalborgEW_PeriodAvg
    #  }
    #  plot <- plot + 
    #    geom_line(data = plotdata, aes_string(x="PeriodAvg", y=input$timeseries_function), color = "black", linetype = "dotted") +
    #    geom_point(data = plotdata, aes_string(x="PeriodAvg", y=input$timeseries_function), color = "black")
    #}
    #plotlyplot <- ggplotly(plot, tooltip = "text")
    #plotlyplot <- plotlyplot %>% 
    #  layout(margin = list(l = 70,
    #                       r = 50,
    #                       b = 50,
    #                       t = 20,
    #                       pad = 4
    #  )
    #  )
    #return(plotlyplot)
    
    ds <- data()[["subset"]]
    ds$tax <- filter(ds$tax, grepl(paste0(as.character(MiF$Genus[which(MiF[,input$timeseries_function] %in% c("POS", "VAR"))]), "$", collapse = "|"), Genus))
    rownames(ds$tax) <- ds$tax$OTU
    ds$abund <- ds$abund[rownames(ds$tax),]
    
    if(input$timeseries_function_group == "avg") {
      group_by <- NULL
    } else if(!input$timeseries_function_group == "avg") {
      group_by <- "Plant"
    }
    plot <- suppressWarnings(amp_timeseries(data = ds,
                                            time_variable = "Date",
                                            tax_aggregate = "Genus",
                                            group_by = group_by,
                                            #plotly = TRUE,
                                            raw = TRUE,
                                            tax_show = input$timeseries_function_tax_show,
                                            split = input$timeseries_function_split
    ))
    plotlyplot <- ggplotly(plot) %>% 
      layout(margin = list(l = 70,
                           r = 50,
                           b = 50,
                           t = 20,
                           pad = 4
      )
      )
    plotlyplot$elementId <- NULL #To suppress warning spam
    return(plotlyplot)
  })
  
  ######################### ORDINATION #########################
  output$ordination <- renderPlotly({
    plot <- amp_ordinate(data()[["subset"]],
                         type = "PCA",
                         transform = "hellinger",
                         sample_color_by = "Plant",
                         sample_colorframe = "Plant",
                         sample_colorframe_label = "Plant",
                         sample_trajectory = if(isTRUE(input$ord_trajectory)) "Date" else NULL,
                         sample_trajectory_group = "Plant",
                         sample_plotly = c("Plant", "Date")
    )
    plotly <- ggplotly(plot) %>%
      layout(showlegend = FALSE)
    plotly$elementId <- NULL #To suppress warning spam
    return(plotly)
  })
  
  ######################### DOWNLOAD HANDLERS #########################
  output$guide_pdf <- downloadHandler(
    filename = "MiDAS dashboard guide.PDF",
    content = function(file) {
      file.copy("app_guide.pdf", file)
    }
  )
})