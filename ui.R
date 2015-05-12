library(shiny)
library(shinyBS)

shinyUI(fluidPage(
  title = "FAOSTAT Trade Matrix Explorer",

                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                                  # Hide the red error messages!!!
                                  tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }",
                                             ".outputRow{height:550px}"
                                  )
                                ), 

                                tags$h1("FAOSTAT Trade Matrix Explorer"),

                                fluidRow(
                                  shiny::column(8, tags$h3("Hello World"), 
                                                tags$i("We could have a general introduction to the application.",
                                                       "We could also state that by clicking the plots you get extra information about the plots.",
                                                       "Also we could write about something else here. Add a link or just wish people a good day")),
                                  shiny::column(4, tags$h3("Dear heavy-users"),
                                                tags$a(href="https://github.com/UNFAOstatistics/trade_matrix/blob/master/README.md","Please download the app and run it locally by following the instruction"))
                                ),
                                tags$hr(),
                                # -----------------------------------------------------------------------------------
                                  fluidRow(
                                    shiny::column(4, radioButtons("dataType", tags$h4("Show"),inline = TRUE, choices = list("Import", "Export", "Both"), selected = "Import"),
                                                  bsTooltip("dataType", "Select Import, Export or both", "bottom", options = list(container = "body"))),
                                    shiny::column(4, uiOutput("reporter_country"),
                                                  bsTooltip("reporter_country", "Currently only top 5 countries per each M49 macro regions by import value in 2011 are displayed", "bottom", options = list(container = "body"))),
                                    shiny::column(4, uiOutput("element"),
                                                  bsTooltip("element", "Either value in 1000 US$ or quantity in tonnes", "bottom", options = list(container = "body")))
                                  ),
                                  fluidRow(
                                    shiny::column(4, uiOutput("year_data"),
                                                  bsTooltip("year_data", "Pick a year for the map & bar chart", "bottom", options = list(container = "body"))),
                                    shiny::column(4, uiOutput("item"),
                                                  bsTooltip("item", "Showing top 20 items based on export/import VALUE of the latest year in the time-series. Items are ordered by value.", "bottom", options = list(container = "body"))),
                                    shiny::column(4, tags$h4("Download in A4 vector-pdf format:"), 
                                                  #radioButtons("graphic_format", "",inline = TRUE, choices = list("vector .pdf", "bitmap .png"), selected = "bitmap .png"),
                                                  downloadButton('dlMap', 'Map'),
                                                  downloadButton('dlTimeseries', 'Time-series by country'),
                                                  downloadButton('dlBarchart', 'Top 30 items')
                                                  )
                                  ),
                                  fluidRow(
                                    shiny::column(12, uiOutput("limit_partner"),
                                                  bsTooltip("limit_partner", "showing maximun of top 20 partner countries by default", "bottom", options = list(container = "body")))
                                  ),
                                  tags$hr(),

                                  tabsetPanel(tabPanel("Map", plotOutput("export_map",height="900px", width="auto")),
                                              tabPanel("Time-series by partner country", plotOutput("export_timeseries",height="500px", width="auto")),
                                              tabPanel("Top 30 items", plotOutput("export_Barchart",height="750px", width="auto"))
                                              #tabPanel("Table", dataTableOutput("mytable"))
                                                                                           ),
                              tags$hr(),
                              tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/logo200.png"),
                              tags$br(),
                              tags$br(),
                              tags$a(href="https://github.com/UNFAOstatistics/trade_matrix", "Code in Github")
)
)