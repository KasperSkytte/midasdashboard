#####################################################
# For this one you need at least 20GB RAM AVAILABLE #
#####################################################

source("data-raw/helper-functions.R")
require("data.table")
metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/biobank/as/metadata_BioBank_2021-11-09.xlsx", detectDates = TRUE)
#this is hopefully FALSE, otherwise manually check which samples to remove:
any(duplicated(metadata$Sample))

#filter a few useless samples
metadata <- filter(metadata, !Sample %chin% paste0("MQ201110-", 309:311))
metadata$Date <- lubridate::ymd(metadata$Date)
metadata$Year <- as.character(lubridate::year(metadata$Date))

#### add seasonal period and week number ####
#extract seasonal periods from dates
WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

# Convert dates from any year to 2012 dates
dates <- as.Date(strftime(metadata$Date, format="2012-%m-%d"))
#extract periods and set factors for correct chronological order
metadata$Period <- ifelse (dates >= WS | dates < SE, "Winter", #winter
                           ifelse (dates >= SE & dates < SS, "Spring", #spring
                                   ifelse (dates >= SS & dates < FE, "Summer", "Fall"))) #summer, fall
metadata$Period <- factor(metadata$Period, levels = c("Spring", "Summer", "Fall", "Winter"))

metadata <- tibble::add_column(metadata,
                               Week = as.character(lubridate::isoweek(metadata$Date)),
                               .after = "Date")

setDT(metadata)
#### fix Plant and ID columns ####
metadata[grepl("extneg", tolower(LibID)), ID := "EXTNEG"]
metadata[grepl("extneg", tolower(LibID)), Plant := "CTRL"]
metadata[grepl("pcrpor", tolower(LibID)), ID := "PCRPOS"]
metadata[grepl("pcrpos", tolower(LibID)), Plant := "CTRL"]
metadata[grepl("pcrneg", tolower(LibID)), ID := "PCRNEG"]
metadata[grepl("pcrneg", tolower(LibID)), Plant := "CTRL"]
metadata <- metadata[!is.na(Plant)] #this removes the weird LibID samples: MQ181023-148, MQ181203-218, MQ181214-138, MQ190919-270
metadata <- metadata[!Sample %chin% "MQ201110-248"]
metadata[ID == "Lynetten", Plant := "Lynetten"]
metadata[ID == "Avedøre", Plant := "Avedøre"]
metadata[grepl("^Marselisborg", Plant), Plant := "Marselisborg"]

metadata[is.na(Line) | Line == "" | Line == "NA", Line := NA]
metadata[!Plant %chin% c("Damhusåen", "Marselisborg"), Line := NA]
metadata[grepl("^LIB-AAW-HC-U", LibID), Line := "U"]
metadata[grepl("^LIB-AAW-HC-O", LibID), Line := "O"]
metadata[, Plant := ifelse(!is.na(Line), paste0(Plant, "-", Line), Plant)]
#metadata[is.na(Plant) & is.na(Line), Plant := ID]

biobank <- amp_load(
  otutable = "data-raw/amplicon_data/biobank/as/ASVtable.tsv",
  metadata = metadata,
  taxonomy = "data-raw/amplicon_data/biobank/as/ASVs.R1.midas481.sintax")
biobank <- amp_subset_samples(biobank, !grepl("ext|pcr|neg|pos", tolower(LibID)) & !Plant %in% "PCRNEG", normalise = TRUE)

#this step uses an awful lot of memory
biobank <- ampvis2:::filter_species(biobank, 0.1)
biobank$metadata <- mutate_at(biobank$metadata, vars(Date), lubridate::ymd)
biobank <- fix_metadata(biobank)

data("mfg_functions")
biobank <- genusfunctions(biobank,
                          function_data = mfg_functions)

biobank_PeriodAvg <- periodAvg(biobank$metadata)

usethis::use_data(biobank, overwrite = TRUE)
usethis::use_data(biobank_PeriodAvg, overwrite = TRUE)


