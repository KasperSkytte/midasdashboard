<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/cmc-aau/MiDAS-app/workflows/R-CMD-check/badge.svg)](https://github.com/cmc-aau/MiDAS-app/actions)
<!-- badges: end -->

# About
midasdashboard is a [Shiny App](https://shiny.rstudio.com/) to browse the microbial community of activated sludge and anaerobic digesters of (mostly) Danish waste water treatment plants. Live at https://eb-aau.shinyapps.io/midasdashboard/.

# Installation
```
# install.packages("devtools")
devtools::install_github("cmc-aau/midasdashboard")
```

# Usage
The Shiny app is bundled into an R package. To run use the `run_app()` function from the package.
```
midasdashboard::run_app()
```

The login is `MiDAS2017` and password `spildevand`
