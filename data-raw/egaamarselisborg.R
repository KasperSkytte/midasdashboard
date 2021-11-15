source("data-raw/helper-functions.R")

#egaamarselisborg CP332 hasteprøver
#data copied from CP-Ongoing 18 may 2018
egaamarselisborg <- amp_subset_samples(
  amp_load(
    otutable = read.delim("data-raw/amplicon_data/aarhusvand CP332/otutable.txt", check.names = FALSE),
    metadata = openxlsx::read.xlsx("data-raw/amplicon_data/aarhusvand CP332/CP332_metadata.xlsx", rows = 1:21, cols = c(1,9,11))),
  normalise = TRUE
)
colnames(egaamarselisborg$metadata) <- c("SampleID", "Plant", "Date")
egaamarselisborg$metadata$Date <- as.Date(egaamarselisborg$metadata$Date, origin = "1899-12-30", format = "%Y-%m-%d")
egaamarselisborg$metadata <- genusfunctions(egaamarselisborg)

usethis::use_data(egaamarselisborg, overwrite = TRUE)
