source("data-raw/helpers.R")

metadata <- openxlsx::read.xlsx("data-raw/amplicon_data/20191112 BioBANK/metadata_BioBank_2019-11-27.xlsx", detectDates = TRUE)
metadata$Line[is.na(metadata$Line) | metadata$Line == "" | metadata$Line == "NA"] <- NA
metadata$Plant[grepl("^Marselisborg", metadata$Plant)] <- "Marselisborg"
metadata <- mutate(metadata, Plant = ifelse(!is.na(Line), paste0(Plant, "-", Line), Plant))
biobank <- amp_load(
  otutable = "data-raw/amplicon_data/20191112 BioBANK/ASVtable.tsv",
  metadata = metadata,
  taxonomy = "data-raw/amplicon_data/20191112 BioBANK/ASVs.R1.midas36.sintax")
biobank <- amp_subset_samples(biobank, !grepl("ext|pcr|neg|pos", tolower(LibID)) & !Plant %in% "PCRNEG")
biobank$metadata <- mutate(biobank$metadata, Plant )
biobank <- ampvis2:::filter_species(biobank, 0.1)
biobank$metadata <- mutate_at(biobank$metadata, vars(Date), lubridate::ymd)
biobank <- fix_metadata(biobank)

data("mfg_functions")
biobank <- genusfunctions(biobank,
                          function_data = mfg_functions)

biobank_PeriodAvg <- periodAvg(biobank$metadata)

usethis::use_data(biobank, overwrite = TRUE)
usethis::use_data(biobank_PeriodAvg, overwrite = TRUE)