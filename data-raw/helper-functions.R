require("ampvis2")
require("tidyverse")

fix_metadata <- function(metadata) {
  colnames(metadata)[1] <- "SampleID"
  if(!any(colnames(metadata) %in% "Date"))
    stop("No 'Date' variable found in metadata", call. = FALSE)
  if(!lubridate::is.Date(metadata$Date))
    stop("Date variable is not in date format")
  #if(any(is.na(metadata$Date)) | any(metadata$Date %in% ""))
  #  stop("dates contain NA values")
  metadata$Year <- as.character(lubridate::year(metadata$Date))
  
  #extract seasonal periods from dates
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  dates <- as.Date(strftime(metadata$Date, format="2012-%m-%d"))
  #extract periods and set factors for correct chronological order
  metadata$Period <- ifelse (dates >= WS | dates < SE, "Vinter", #winter
                                  ifelse (dates >= SE & dates < SS, "Forår", #spring
                                          ifelse (dates >= SS & dates < FE, "Sommer", "Efterår"))) #summer, fall
  metadata$Period <- factor(metadata$Period, levels = c("Forår", "Sommer", "Efterår", "Vinter"))
  
  #check variables
  metadata <- metadata[,which(tolower(colnames(metadata)) %in% c(
    "sampleid",
    "plant",
    "year",
    "period",
    "date",
    "svi",
    "dsvi",
    "reaktor",
    "line",
    "temperature")), drop = FALSE]
  metadata$Plant <- trimws(metadata$Plant)
  metadata$Plant <- stringr::str_replace_all(
    metadata$Plant,
    pattern = c(
      "Aalborg E" = "Aalborg East",
      "Aalborg W" = "Aalborg West",
      "Aalborg oest" = "Aalborg East",
      "Aalborg Øst" = "Aalborg East",
      "Aalborg Vest" = "Aalborg West",
      "Esbjerg E" = "Esbjerg East",
      "Esbjerg W" = "Esbjerg West",
      "Esbjerg Vest" = "Esbjerg West",
      "aaby" = "Åby",
      "Odense NV" = "Odense NW",
      "Avedoere" = "Avedøre",
      "Damhusaaen" = "Damhusåen",
      "Egaa" = "Egå",
      "Ejby Moelle" = "Ejby Mølle",
      "Fornaes" = "Fornæs",
      "Hjoerring" = "Hjørring",
      "Naestved" = "Næstved",
      "Ringkoebing" = "Ringkøbing",
      "Soeholt" = "Søholt",
      "Sohoelt" = "Søholt"
    )
  )
  return(metadata)
}

genusfunctions <- function(
  data,
  functions = c(
    "FIL",
    "AOB", 
    "NOB",
    "PAO",
    "GAO",
    "MET", 
    "ACE",
    "FER",
    "DN", 
    "Anammox"
  ),
  function_data = MiF
) {
  data <- ampvis2:::amp_rename(data)
  message("Normalising counts...")
  data <- ampvis2:::normaliseTo100(data)
  MiDAS_functions <- list()
  
  for(i in 1:length(functions)) {
    MiDAS_functions[[functions[i]]] <- data
    MiDAS_functions[[functions[i]]]$tax$Genus[which(MiDAS_functions[[functions[i]]]$tax$Genus %in% as.character(function_data$Genus[which(function_data[,functions[i]] %in% c("POS"))]))] <- functions[i]
    if(any(MiDAS_functions[[functions[i]]]$tax$Genus %in% functions[i])) {
      message(paste0("Calculating ", functions[i], "..."))
      MiDAS_functions[[functions[i]]] <- suppressMessages(ampvis2::amp_subset_taxa(MiDAS_functions[[functions[i]]], tax_vector = functions[i]))
      MiDAS_functions[[functions[i]]][["result"]] <- t(ampvis2::amp_heatmap(MiDAS_functions[[functions[i]]], tax_aggregate = "Genus", tax_show = "all", normalise = FALSE, textmap = TRUE))
      MiDAS_functions[[functions[i]]][["result"]] <- data.frame("SampleID" = as.character(rownames(MiDAS_functions[[functions[i]]][["result"]])), MiDAS_functions[[functions[i]]][["result"]])
      data$metadata <- merge(data$metadata, MiDAS_functions[[functions[i]]][["result"]], by = 1)
    }
  }
  rownames(data$metadata) <- data$metadata[,1]
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("FIL"))] <- "Filamentous"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("AOB"))] <- "Ammonia.Oxidizing.Bacteria"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("NOB"))] <- "Nitrite.Oxidizing.Bacteria"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("PAO"))] <- "Polyphosphate.Accumulating.Organisms"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("GAO"))] <- "Glygogen.Accumulating.Organisms"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("ACE"))] <- "Acetogens"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("FER"))] <- "Fermenters"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("DN"))] <- "Denitrifiers"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("MET"))] <- "Methanogens"
  colnames(data$metadata)[which(colnames(data$metadata) %in% c("Anammox"))] <- "Anammox"
  return(data)
}

periodAvg <- function(metadata, datecol = "Date") {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(metadata[,datecol], format="2012-%m-%d"))
  
  metadata$PeriodAvg <- ifelse (d >= WS | d < SE, "2000-12-15", #winter
                                ifelse (d >= SE & d < SS, "2000-03-15", #spring
                                        ifelse (d >= SS & d < FE, "2000-06-15", "2000-09-15"))) #summer, fall
  metadata$PeriodAvg <- paste0(metadata$Year, "-", lubridate::month(metadata$PeriodAvg), "-", lubridate::day(metadata$PeriodAvg)) %>% lubridate::ymd()
  metadata <- group_by(metadata, PeriodAvg)
  metadata <- suppressWarnings(summarise_all(metadata, mean, na.rm = TRUE))
  metadata <- metadata[,apply(metadata, 2, function(x) !all(is.na(x)))]
  return(metadata)
}