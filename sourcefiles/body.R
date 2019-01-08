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
                      value = if(exists("testmode")) "MiDAS2017" else ""
            ),
            passwordInput("password",
                          "Password:",
                          value = if(exists("testmode")) "spildevand" else ""
            ),
            actionButton("login_button",
                         "Log ind",
                         icon("sign-in")
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
      DT::dataTableOutput("plantinfo")
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
                  tags$a(href = "http://www.bxhorn.com/Downloads/RColors1_bxhorn.pdf", target = "_blank", "Se mulige farver"),
                  tags$hr(),
                  checkboxInput(inputId = "heatmap_plot_values",
                                label = "Vis procenttal", 
                                value = TRUE
                  ),
                  checkboxInput(inputId = "heatmap_showmean",
                                label = "Vis gennemsnit af alle anlæg i MiDAS", 
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
                                 icon = icon("refresh")
                    )
                  ),
                  conditionalPanel(
                    condition="!$('html').hasClass('shiny-busy')", 
                    actionButton(inputId = "render_heatmap",
                                 label = "Opdater plot"
                    )
                  )
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
      tabName = "timeseries_genus",
      fluidRow(
        width = 12,
        column(
          width = 9,
          box(
            title = tagList(shiny::icon("line-chart"), "Mest hyppige bakterier (Genus)"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("timeseries",
                         height = "700px")
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
            radioButtons(inputId = "timeseries_group",
                               label = "Vis som",
                               choices = c("Gennemsnit af valgte anlæg (per dato)" = "avg", "Farvet efter individuelle anlæg" = "grouped"),
                               selected = "grouped"
                               ),
            sliderInput(inputId = "timeseries_tax_show",
                        label = "Antal af de mest hyppige bakterier (Genus niveau)",
                        min = 1,
                        max = 16,
                        value = 4,
                        step = 1,
                        width = "300px"
                        ),
            checkboxInput(inputId = "timeseries_split",
                          label = "Individuelle plots",
                          value = TRUE
                          ),
            tags$hr(),
            conditionalPanel(
              condition="$('html').hasClass('shiny-busy')", 
              actionButton(inputId = "alejkbrnrtcfhge",
                           label = "Opdater plot",
                           icon = icon("refresh")
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
      tabName = "timeseries_function",
      fluidRow(
        width = 12,
        column(
          width = 9,
          box(
            title = tagList(shiny::icon("line-chart"), "Genus funktion"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("timeseries_function",
                         height = "500px")
          ),
          conditionalPanel(
            condition = "(input.dataset == 'Aktivt slam' || input.dataset == 'Aalborg BioBANK (2012-2015)') && input.timeseries_function == 'FIL'",
            tabBox(
              title = tagList(shiny::icon("line-chart"), "Slam Volumen Index"),
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
            selectInput(inputId = "timeseries_function",
                        label = "Genus funktion",
                        choices = c("Trådformende" = "FIL",
                                    "Ammonium oxiderende bakterier (AOB)" = "AOB",
                                    "Nitrit oxiderende bakterier (NOB)" = "NOB",
                                    "Polyfosfat akkumulerende bakterier (PAO)" = "PAO",
                                    "Glykogen akkumulerende bakterier (GAO)" = "GAO",
                                    "Acetat producerende bakterier" = "ACE",
                                    "Fermenterende bakterier" = "FER",
                                    "Denitrificerende bakterier" = "DN",
                                    "Metan producerende (archaea)" = "MET"
                        ),
                        selected = "Filamentous",
                        multiple = FALSE
            ),
            radioButtons(inputId = "timeseries_function_group",
                         label = "Vis som",
                         choices = c("Gennemsnit af valgte anlæg (per dato)" = "avg", "Farvet efter individuelle anlæg" = "grouped"),
                         selected = "avg"
            ),
            sliderInput(inputId = "timeseries_function_tax_show",
                        label = "Antal af de mest hyppige bakterier (Genus niveau)",
                        min = 1,
                        max = 16,
                        value = 6,
                        step = 1,
                        width = "300px"
            ),
            checkboxInput(inputId = "timeseries_function_split",
                          label = "Individuelle plots",
                          value = FALSE
            ),
            conditionalPanel(
              condition = "input.timeseries_function == 'FIL'",
              checkboxInput(
                input = "timeseries_function_avg",
                label = "Vis årstidsgennemsnit af alle anlæg (sort)",
                value = TRUE
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
              label = "Indtegn bane mellem punkterne efter dato",
              value = TRUE
            )
            )
        )
      )
    )
  )
)