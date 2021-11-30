source("data-raw/helper-functions.R")

metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/[2018-12-3] data/MiDAS/metadata_ShinyApp_2018.xlsx", detectDates = TRUE)
otutable <- data.table::fread("data-raw/amplicon_data/[2018-12-3] data/MiDAS/otutable.txt", fill = TRUE)
MiDAS <- amp_load(otutable, metadata)
MiDAS <- ampvis2:::filter_species(MiDAS, 0.1)
MiDAS$metadata <- fix_metadata(MiDAS$metadata)
MiDAS <- genusfunctions(MiDAS)
usethis::use_data(MiDAS, overwrite = TRUE)

#MiDAS_PeriodAvg <- periodAvg(MiDAS$metadata)
#MiDAS_PeriodAvg$Date <- as.character(as.Date(MiDAS_PeriodAvg$Date))
#usethis::use_data(MiDAS_PeriodAvg, overwrite = TRUE)
