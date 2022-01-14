source("data-raw/helper-functions.R")

# archaea #
metadata <- openxlsx::read.xlsx(
  "data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/archea_V3V5/metadata_midad_archaea.xlsx", 
  sheet = 2, 
  detectDates = TRUE
)
metadata <- filter(metadata, !Plant %in% c("POS CTRL", "NEG CTRL"))
metadata$Date <- lubridate::ymd(metadata$Date)
otutable <- data.table::fread("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/archea_V3V5/otutable.txt", fill = TRUE)
midad_arc <- amp_load(otutable, metadata)
midad_arc <- ampvis2:::filter_species(midad_arc, 0.1)
midad_arc$metadata <- fix_metadata(midad_arc$metadata)
midad_arc$metadata$Reaktor[which(is.na(midad_arc$metadata$Reaktor) | midad_arc$metadata$Reaktor == "")] <- NA
midad_arc$metadata <- mutate(midad_arc$metadata, Plant = paste0(Plant, " R-", Reaktor))
midad_arc <- genusfunctions(midad_arc)
#midad_arc_PeriodAvg <- periodAvg(midad_arc$metadata)
#usethis::use_data(midad_arc_PeriodAvg, overwrite = TRUE)
midad_arc <- amp_subset_taxa(
  midad_arc,
  "Archaea"
)

usethis::use_data(midad_arc, overwrite = TRUE)

# bacteria #
metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/bacteria_V1V3/metadata_midad_bacteria_with_add_files.xlsx",
                                sheet = "metadata", 
                                detectDates = TRUE)
colnames(metadata)[7] <- "Reaktor" #"Reactor" = "Reaktor"
metadata$Date <- lubridate::ymd(metadata$Date)
otutable <- data.table::fread("data-raw/amplicon_data/MiDAD_Quarterly_samples_2006-2018_OTUs/bacteria_V1V3/otutable.txt", fill = TRUE)
midad_bac <- amp_load(otutable, metadata)
midad_bac <- ampvis2:::filter_species(midad_bac, 0.1)
midad_bac$metadata <- fix_metadata(midad_bac$metadata)
midad_bac$metadata$Reaktor[which(is.na(midad_bac$metadata$Reaktor) | midad_bac$metadata$Reaktor == "")] <- NA
midad_bac$metadata <- mutate(midad_bac$metadata, Plant = paste0(Plant, " R-", Reaktor))
midad_bac <- genusfunctions(midad_bac)
midad_bac <- amp_subset_taxa(
  midad_bac,
  "Bacteria"
)
#midad_bac_PeriodAvg <- periodAvg(midad_bac$metadata)
#usethis::use_data(midad_bac_PeriodAvg, overwrite = TRUE)
usethis::use_data(midad_bac, overwrite = TRUE)