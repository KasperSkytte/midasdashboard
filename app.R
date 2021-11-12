# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

options(rsconnect.max.bundle.files = 30000,
        rsconnect.max.bundle.size = 3145728000)

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
midasdashboard::run_app() # add parameters here (if any)
