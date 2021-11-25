source("data-raw/helper-functions.R")
metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/bacteria_V1V3/metadata_midad_bacteria_with_add_files.xlsx",
                                sheet = "metadata", 
                                detectDates = TRUE)
colnames(metadata)[7] <- "Reaktor" #"Reactor" = "Reaktor"
metadata$Date <- lubridate::ymd(metadata$Date)
otutable <- data.table::fread("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/bacteria_V1V3/otutable.txt", fill = TRUE)
digester_bacteria <- amp_load(otutable, metadata)
digester_bacteria <- ampvis2:::filter_species(digester_bacteria, 0.1)
digester_bacteria$metadata <- fix_metadata(digester_bacteria$metadata)
digester_bacteria$metadata$Reaktor[which(is.na(digester_bacteria$metadata$Reaktor) | digester_bacteria$metadata$Reaktor == "")] <- NA
digester_bacteria$metadata <- mutate(digester_bacteria$metadata, Plant = paste0(Plant, " R-", Reaktor))
digester_bacteria <- genusfunctions(digester_bacteria)

digester_bacteria_PeriodAvg <- periodAvg(digester_bacteria$metadata)

usethis::use_data(digester_bacteria, overwrite = TRUE)
usethis::use_data(digester_bacteria_PeriodAvg, overwrite = TRUE)
