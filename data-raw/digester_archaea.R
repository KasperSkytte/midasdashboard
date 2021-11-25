source("data-raw/helper-functions.R")
metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/archea_V3V5/metadata_midad_archaea.xlsx", 
                                sheet = 2, 
                                detectDates = TRUE)
metadata <- filter(metadata, !Plant %in% c("POS CTRL", "NEG CTRL"))
metadata$Date <- lubridate::ymd(metadata$Date)
otutable <- data.table::fread("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/archea_V3V5/otutable.txt", fill = TRUE)
digester_archaea <- amp_load(otutable, metadata)
digester_archaea <- ampvis2:::filter_species(digester_archaea, 0.1)
digester_archaea$metadata <- fix_metadata(digester_archaea$metadata)
digester_archaea$metadata$Reaktor[which(is.na(digester_archaea$metadata$Reaktor) | digester_archaea$metadata$Reaktor == "")] <- NA
digester_archaea$metadata <- mutate(digester_archaea$metadata, Plant = paste0(Plant, " R-", Reaktor))
digester_archaea <- genusfunctions(digester_archaea)

digester_archaea_PeriodAvg <- periodAvg(digester_archaea$metadata)

usethis::use_data(digester_archaea, overwrite = TRUE)
usethis::use_data(digester_archaea_PeriodAvg, overwrite = TRUE)
