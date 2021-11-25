MiF_midasfieldguide <- ampvis2:::extractFunctions(ampvis2:::getMiDASFGData())
MiF_midasfieldguide <- MiF_midasfieldguide[
  ,
  c(
    "Genus",
    "Filamentous",
    "AOB",
    "NOB",
    "PAO",
    "GAO",
    "Methanogen",
    "Acetogen",
    "Fermentation",
    "Nitrite reduction", 
    "Anammox"
  )
]
colnames(MiF_midasfieldguide) <- c(
  "Genus",
  "FIL",
  "AOB", 
  "NOB",
  "PAO",
  "GAO",
  "MET", 
  "ACE",
  "FER",
  "DN", 
  "Anammox"
)
usethis::use_data(mfg_functions, overwrite = TRUE)
