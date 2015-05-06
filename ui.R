# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library(shiny)

#shinyUI(fluidPage(
shinyUI(navbarPage("FAOSTAT Trade Matrix Explorer", id="nav",
                   
                   tabPanel("Exports",
                            div(class="outer",
                   
#                    tabPanel("",
#                             div(class="inner",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css")#,
                                  #                 # Hide the red error messages!!!
#                                   tags$style(type="text/css",
#                                              ".shiny-output-error { visibility: hidden; }",
#                                              ".shiny-output-error:before { visibility: hidden; }",
#                                              ".outputRow{height:550px}"
#                                   )
                                ), 

                                tags$h3("Export map"),
                                plotOutput("export_map", width="100%", height = "100%"),
#                                 tags$h3("Import map"),
#                                 plotOutput("export_map2", width="100%", height = "50%"),

                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 350, height = "auto",
                                              
                                              tags$h3("Select data"),
                                              uiOutput("reporter_country"),
                                              uiOutput("item"),
                                              uiOutput("element"),
                                              uiOutput("year_data"),
                                              radioButtons("dataType", label = "Show",inline = TRUE,
                                                           choices = list("Import", 
                                                                          "Export",
                                                                          "Both"),
                                                           selected = "Import"),
                                              downloadButton('dlMap', 'Save map in A4 vector pdf')
                                )#,



                                
                                 #tags$h1("FAOSTAT Trade Matrix Explorer")#,
#                                 tags$hr(),
#                                 fluidRow(
#                                   shiny::column(4, tags$h4("Hello world"), 
#                                                 tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
#                                   shiny::column(4, tags$br(),tags$br(),tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
#                                   shiny::column(4, tags$br(),tags$br(),tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/logo200.png"))
#                                 ),
#                                 tags$h3("Select data"),
#                                 fluidRow(
#                                   shiny::column(4, uiOutput("reporter_country")),
#                                   shiny::column(4, uiOutput("item")),
#                                   shiny::column(4, uiOutput("element"))
#                                 ),
# #                                 fluidRow(
# #                                   shiny::column(5, dataTableOutput("mytable"))
# #                                 ),
#                                 tags$hr(),
#                                 tags$h3("Exports"),
# #                                 fluidRow(
# #                                   shiny::column(12, plotOutput("export_map"))
# #                                   ),
# #                                 fluidRow(
# #                                   shiny::column(12, uiOutput("year_export"))
# #                                 ),
#                                 tags$hr(),
#                                 tags$h3("Imports"),
#                                 fluidRow(
#                                   shiny::column(9, plotOutput("import_map")),
#                                   shiny::column(3, uiOutput("year_import"))#, 
# #                                   shiny::column(9, plotOutput("bar")),
# #                                   shiny::column(3, uiOutput("yearBar"),
# #                                                 uiOutput("countryBar"), 
# #                                                 downloadButton('dlBar', 'Download plot'),
# #                                                 tags$br(),
# #                                                 downloadButton('dlDataBar', 'Download data'))
# #                                 ),
# #                                 tags$hr(),
# #                                 tags$h3("Map"),
# #                                 fluidRow(
# #                                   shiny::column(9, plotOutput("map")),
# #                                   shiny::column(3, uiOutput("yearMap"), 
# #                                                 downloadButton('dlMap', 'Download plot'),
# #                                                 tags$br(),
# #                                                 downloadButton('dlDataMap', 'Download data'))

))))