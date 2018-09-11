sidebar <- dashboardSidebar(
  width = 270,
  sidebarMenuOutput("sidebarUI")
  ,tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)