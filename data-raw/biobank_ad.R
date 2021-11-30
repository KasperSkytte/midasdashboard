source("data-raw/helper-functions.R")
require("data.table")

digester_data <- function(
  otutable_path = "data-raw/amplicon_data/biobank/ad/ASVtable.tsv",
  metadata_path,
  metadata_cols = c(
    "Seq_ID" = "SampleID",
    "SampleSite" = "Plant",
    "SampleDate" = "Date",
    "Digester_Type" = "Type"
  )
) {
  metadata <- fread(
    metadata_path,
    encoding = "Latin-1",
    select = names(metadata_cols),
    col.names = metadata_cols
  )
  d <- amp_load(
    otutable = otutable_path,
    metadata = metadata[!apply(metadata == "", 1, all)], #removes blank rows
    taxonomy = "data-raw/amplicon_data/biobank/ad/ASVs.R1.midas481.sintax"
  )
  #check controls and remove
  amp_subset_samples(
    d,
    tolower(Plant) %chin% "aau",
    normalise = FALSE
  ) %>% 
    amp_heatmap(normalise = FALSE)
  d <- amp_subset_samples(
    d,
    !tolower(Plant) %chin% "aau",
    normalise = TRUE,
    minreads = 1000,
    removeAbsents = TRUE
  )
  #add/rename reactor column
  #append reactor to plant
  d <- ampvis2:::filter_species(d, 0.1)
  d$metadata$Date <- lubridate::ymd(d$metadata$Date)
  d$metadata <- fix_metadata(d$metadata)
  d <- genusfunctions(d)
  
  invisible(d)
}

## archaea
biobank_ad_arc <- digester_data(
  metadata_path = "data-raw/amplicon_data/biobank/ad/211116_metadataBioBank_Archaea.txt",
)
usethis::use_data(biobank_ad_arc, overwrite = TRUE)

## bacteria
biobank_ad_bac <- digester_data(
  metadata_path = "data-raw/amplicon_data/biobank/ad/211116_metadataBioBank_Bacteria.txt"
)
usethis::use_data(biobank_ad_bac, overwrite = TRUE)
