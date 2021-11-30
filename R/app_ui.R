#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom plotly plotlyOutput
#' @importFrom DT dataTableOutput
#' @noRd
app_ui <- function(request) {
  sidebar <- dashboardSidebar(
    width = 270,
    sidebarMenuOutput("sidebarUI")
    ,tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
    )
  )
  header <- dashboardHeader(
    title = tagList(a(href="http://midasfieldguide.org", target = "_blank", style="color:#FFFFFF", img(src = "www/logosmall.png"), "MiDAS dashboard")),
    titleWidth = 270
  )
  body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "login",
        fluidRow(
          width = 8,
          column(
            width = 3,
            wellPanel(
              h3("Log ind"),
              tags$hr(),
              textInput("user_name",
                        "Bruger:",
                        value = if(isTRUE(getOption("testmode"))) "MiDAS2017" else ""
              ),
              passwordInput("password",
                            "Password:",
                            value = if(isTRUE(getOption("testmode"))) "spildevand" else ""
              ),
              actionButton("login_button",
                           "Log ind",
                           icon("sign-in-alt")
              )
            )
          ),
          column(
            width = 5,
            uiOutput("logintext")
          )
        )
      ),
      tabItem(
        tabName = "plantinfo",
        dataTableOutput("plantinfo")
      ),
      tabItem(tabName = "community",
              fluidRow(
                width = 12,
                column(
                  width = 9,
                  box(
                    title = tagList(shiny::icon("list-ol"), "Heatmap"),
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("heatmap", height = "620px")
                  )
                ),
                column(
                  width = 3,
                  box(
                    title = tagList(shiny::icon("cog"), "Indstillinger"),
                    width = 12,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    selectizeInput(
                      inputId = "heatmap_group_by",
                      label = "Gruppér",
                      choices = c("Anlæg" = "Plant", "År" = "Year", "Årstid" = "Period", "Dato" = "Date"),
                      selected = "Anlæg",
                      multiple = FALSE,
                      width = "200px"
                    ),
                    selectizeInput(
                      inputId = "heatmap_facet_by",
                      label = "Inddel",
                      choices = c("ingen" = "none", c("Anlæg" = "Plant", "År" = "Year", "Årstid" = "Period")),
                      selected = "Årstid",
                      multiple = FALSE,
                      width = "200px"
                    ),
                    uiOutput("heatmap_sort"),
                    sliderInput(inputId = "heatmap_tax_show",
                                label = "Antal af de mest hyppige bakterier (Genus niveau)",
                                min = 1,
                                max = 50,
                                value = 20,
                                step = 1,
                                width = "300px"
                    ),
                    textInput(inputId = "heatmap_colorvector",
                              label = "Farvegradient (log10)",
                              value = "skyblue3, whitesmoke, salmon2"),
                    downloadLink("RColorSheet", "Se mulige farver"),
                    tags$hr(),
                    checkboxInput(inputId = "heatmap_plot_values",
                                  label = "Vis procenttal", 
                                  value = TRUE
                    ),
                    checkboxInput(inputId = "heatmap_showmean",
                                  label = "Vis gennemsnit af alle anlæg i data sæt",
                                  value = FALSE
                    ),
                    checkboxInput(inputId = "heatmap_function",
                                  label = "Vis funktionel information",
                                  value = FALSE),
                    conditionalPanel(
                      condition = "input.heatmap_facet_by != 'none'",
                      checkboxInput(inputId = "heatmap_turnfacettext",
                                    label = "Vend tekst ved inddeling",
                                    value = FALSE)
                    ),
                    tags$hr(),
                    conditionalPanel(
                      condition="$('html').hasClass('shiny-busy')", 
                      actionButton(inputId = "alejkbrnbc,maenblrtcfhge",
                                   label = "Opdater plot",
                                   icon = icon("sync")
                      )
                    ),
                    conditionalPanel(
                      condition="!$('html').hasClass('shiny-busy')", 
                      actionButton(inputId = "render_heatmap",
                                   label = "Opdater plot"
                      )
                    ),
                    tags$hr(),
                    downloadButton("saveTextmap", "Eksportér i Excel format")
                  ),
                  conditionalPanel(
                    condition = "input.heatmap_function",
                    box(
                      title = "Forkortelser",
                      width = 12,
                      status = "info",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      p("FIL: Filamentous (Trådformende)"),
                      p("AOB: Ammonium oxiderende bakterier"),
                      p("NOB: Nitrit oxiderende bakterier"),
                      p("PAO: Polyfosfat akkumulerende bakterier"),
                      p("GAO: Glykogen akkumulerende bakterier"),
                      p("ACE: Acetat producerende bakterier"),
                      p("FER: Fermenterende bakterier"),
                      p("DN: Denitrificerende bakterier"),
                      p("MET: Metan producerende archaea")
                    )
                  )
                )
              )
      ),
      tabItem(
        tabName = "timeseries",
        fluidRow(
          width = 12,
          column(
            width = 9,
            box(
              title = tagList(shiny::icon("chart-line"), "Tidsserier"),
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("timeseries",
                           height = "700px")
            ),
            conditionalPanel(
              condition = "(input.dataset == 'MiDAS kvartalsprøver AS' || input.dataset == 'BioBank AS') && input.genusfunction == 'FIL'",
              tabBox(
                title = tagList(shiny::icon("chart-line"), "Slam Volumen Index"),
                width = 12,
                side = "right",
                tabPanel(
                  title = "Fortyndet Slam Volumen Index (FSVI)",
                  plotlyOutput("timeseries_dsvi",
                               height = "300px")
                ),
                tabPanel(
                  title = "Slam Volumen Index (SVI)",
                  plotlyOutput("timeseries_svi",
                               height = "300px")
                )
              )
            )
          ),
          column(
            width = 3,
            box(
              title = tagList(shiny::icon("cog"), "Indstillinger"),
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              sliderInput(inputId = "timeseries_tax_show",
                          label = "Antal af de mest hyppige bakterier (Genus niveau)",
                          min = 1,
                          max = 16,
                          value = 9,
                          step = 1,
                          width = "300px"
              ),
              checkboxInput(inputId = "timeseries_freey",
                            label = "Frie y-akser",
                            value = FALSE),
              tags$hr(),
              conditionalPanel(
                condition="$('html').hasClass('shiny-busy')", 
                actionButton(inputId = "alejkbrnrtcfhge",
                             label = "Opdater plot",
                             icon = icon("sync")
                )
              ),
              conditionalPanel(
                condition="!$('html').hasClass('shiny-busy')", 
                actionButton(inputId = "render_timeseries",
                             label = "Opdater plot"
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "ordination",
        fluidRow(
          width = 12,
          column(
            width = 9,
            box(
              width = 12,
              title = tagList(shiny::icon("sitemap"), "Principal Components Analysis"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("ordination",
                           height = "600px"
                           #,width = "600px"
              )
            )
          ),
          column(
            width = 3,
            box(
              title = tagList(shiny::icon("cog"), "Indstillinger"),
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              checkboxInput(
                inputId = "ord_trajectory",
                label = "Indtegn bane gennem punkterne efter dato",
                value = FALSE
              ),
              tags$hr(),
              conditionalPanel(
                condition="$('html').hasClass('shiny-busy')", 
                actionButton(inputId = "alejkbrnrtcfhge",
                             label = "Opdater plot",
                             icon = icon("sync")
                )
              ),
              conditionalPanel(
                condition="!$('html').hasClass('shiny-busy')", 
                actionButton(inputId = "render_ordination",
                             label = "Opdater plot"
                )
              )
            )
          )
        )
      )
    )
  )
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(header, sidebar, body, title = "MiDAS dashboard")
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'midasdashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

