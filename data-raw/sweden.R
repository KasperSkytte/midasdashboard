require("data.table")
source("data-raw/helper-functions.R")
metadata <- openxlsx::read.xlsx(
  "data-raw/amplicon_data/sweden/metadata_midas_sweden_2022-01-13_SHH.xlsx",
  detectDates = TRUE
)

#this is hopefully FALSE, otherwise manually check which samples to remove:
any(duplicated(metadata[[1]]))

#several columns are empty all the way down, just remove
emptycols <- lapply(metadata, function(x) all(is.na(x)))
metadata <- metadata[,!unlist(emptycols)]

#remove control samples and empty rows
setDT(metadata)
metadata[grepl("extneg|pcrpos|pcrnpos|pcrneg", tolower(LibID)), Plant := "CTRL"]
metadata <- metadata[!is.na(LibID)]

#append Line ID to Plant ID
metadata[!Plant %chin% "CTRL", Plant := ifelse(!is.na(Line), paste0(Plant, "-", Line), Plant)]

biobanksweden <- amp_load(
  otutable = "data-raw/amplicon_data/sweden/ASVtable.tsv",
  metadata = metadata,
  taxonomy = "data-raw/amplicon_data/sweden/ASVs.R1.midas481.sintax"
)

#check control samples before removing them
#check controls and remove
amp_subset_samples(
  biobanksweden,
  Plant %chin% "CTRL",
  normalise = FALSE
) %>% 
  amp_heatmap(normalise = FALSE, group_by = "LibID")
biobanksweden <- amp_subset_samples(
  biobanksweden,
  !Plant %chin% "CTRL",
  normalise = TRUE
)

#this step uses an awful lot of memory
biobanksweden <- ampvis2:::filter_species(biobanksweden, 0.1)

biobanksweden$metadata$Date <- lubridate::ymd(biobanksweden$metadata$Date)

data("mfg_functions")
#bacteria
biobanksweden_bac <- biobanksweden %>% 
  amp_subset_samples(
    Primer %chin% "Bacteria"
  ) %>% 
  amp_subset_taxa(
    "k__Bacteria"
  ) %>% 
  genusfunctions(
    function_data = mfg_functions
  )
biobanksweden_bac$metadata <- fix_metadata(biobanksweden_bac$metadata)

#archaea
biobanksweden_arc <- biobanksweden %>% 
  amp_subset_samples(
    Primer %chin% "Archaea"
  ) %>% 
  amp_subset_taxa(
    "k__Archaea"
  ) %>% 
  genusfunctions(
    function_data = mfg_functions
  )
biobanksweden_arc$metadata <- fix_metadata(biobanksweden_arc$metadata)

usethis::use_data(biobanksweden_bac, overwrite = TRUE)
usethis::use_data(biobanksweden_arc, overwrite = TRUE)
#biobanksweden_arc_PeriodAvg <- periodAvg(biobanksweden_arc$metadata)
#biobanksweden_bac_PeriodAvg <- periodAvg(biobanksweden_bac$metadata)

#usethis::use_data(biobanksweden_bac_PeriodAvg, overwrite = TRUE)
#usethis::use_data(biobanksweden_arc_PeriodAvg, overwrite = TRUE)
