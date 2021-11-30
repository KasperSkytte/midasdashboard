source("data-raw/helper-functions.R")

metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/[2018-12-3] data/biobank time series/AAV/metadata_BIOBANK_Aalborg_Vest_2.xlsx", detectDates = TRUE)
otutable <- data.table::fread("data-raw/amplicon_data/[2018-12-3] data/biobank time series/AAV/otutable.txt", fill = TRUE)
AalborgW <- amp_load(otutable, metadata)
AalborgW <- ampvis2:::filter_species(AalborgW, 0.1)
AalborgW$metadata <- fix_metadata(AalborgW$metadata)
AalborgW <- genusfunctions(AalborgW)

##### DSVI #####
AalborgW_DSVI <- openxlsx::read.xlsx("data-raw/amplicon_data/[2018-12-3] data/biobank time series/AAV/DSVI_BIOBANK_AalborgVest_2015-2018.xlsx", detectDates = TRUE)
usethis::use_data(AalborgW, overwrite = TRUE)

#AalborgW_PeriodAvg <- periodAvg(AalborgW$metadata)
#usethis::use_data(AalborgW_PeriodAvg, overwrite = TRUE)

