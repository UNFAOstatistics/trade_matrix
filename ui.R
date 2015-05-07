# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library(shiny)

shinyUI(fluidPage(
  title = "FAOSTAT Trade Matrix Explorer",
#shinyUI(navbarPage("FAOSTAT Trade Matrix Explorer", id="nav",
                   
#                   tabPanel("Exports",
#                            div(class="outer",
                   
#                    tabPanel("",
#                             div(class="inner",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css")#,
                                                  # Hide the red error messages!!!
#                                   tags$style(type="text/css",
#                                              ".shiny-output-error { visibility: hidden; }",
#                                              ".shiny-output-error:before { visibility: hidden; }",
#                                              ".outputRow{height:550px}"
#                                   )
                                ), 

                                
                                tags$h1("FAOSTAT Trade Matrix Explorer"),
                                #plotOutput("export_map", height="700px", width="auto"),


                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    tags$h4("Hello world"), 
                                    tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero."),
                                    tags$br(),
                                    uiOutput("reporter_country"),
                                    uiOutput("item"),
                                    uiOutput("element"),
                                    tags$hr(),
                                    radioButtons("dataType", label = "Show",inline = TRUE, choices = list("Import", "Export", "Both"), selected = "Import"),
                                    uiOutput("year_data"),
                                    downloadButton('dlMap', 'Save map in A4 vector pdf'),
                                    downloadButton('dlTimeseries', 'Save time-series in A4 vector pdf'),
                                    tags$br(),
                                    tags$br(),
                                    tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero."),
                                    tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/logo200.png")
                                  ),
                                  
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Map", plotOutput("export_map",height="600", width="auto"),
                                               plotOutput("sumLine",height="250px", width="auto")),
                                      tabPanel("Time-series", plotOutput("export_timeseries",height="700px", width="auto")),
                                      tabPanel("Table", dataTableOutput("mytable"))
                                      
                                      
                                    )
                                  )
                                )
#                                 fluidRow(
#                                   shiny::column(4, tags$h4("Hello world"), 
#                                                 tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
#                                   shiny::column(4, tags$br(),tags$br(),tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
#                                   shiny::column(4, tags$br(),tags$br(),tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/logo200.png"))
#                                 )


#                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
#                                               top = "auto", left = 50, right = "auto", bottom = 30,
#                                               width = 1000, height = "auto",
#                                               
#                                               fluidRow(
#                                                 shiny::column(4, tags$h4("Hello world"), 
#                                                               tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
#                                                 shiny::column(4, tags$br(),tags$br(),tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
#                                                 shiny::column(4, tags$br(),tags$br(),tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/logo200.png"))
#                                               )
#                                 )#,
  




#                                 tags$hr(),
#                                 fluidRow(
#                                   shiny::column(4, tags$h3("Select data")),
#                                   shiny::column(4, tags$h3("Tweak the plot")),
#                                   shiny::column(4, tags$h3("Dornload plot"))
#                                 ),
#                                 fluidRow(
#                                   shiny::column(4, uiOutput("reporter_country")),
#                                   shiny::column(4, radioButtons("dataType", label = "Show",inline = TRUE, choices = list("Import", "Export", "Both"), selected = "Import")),
#                                   shiny::column(4, downloadButton('dlMap', 'Save map in A4 vector pdf'))
#                                 ),
#                                 fluidRow(
#                                   shiny::column(4, uiOutput("item")),
#                                   shiny::column(4, uiOutput("year_data")),
#                                   shiny::column(4)
#                                 ),
#                                 fluidRow(
#                                   shiny::column(4, uiOutput("element")),
#                                   shiny::column(4),
#                                   shiny::column(4)
#                                 )

#                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
#                                               top = 60, left = "auto", right = 20, bottom = "auto",
#                                               width = 350, height = "auto",
#                                               
#                                               tags$h3("Select data"),
#                                               uiOutput("reporter_country"),
#                                               uiOutput("item"),
#                                               uiOutput("element"),
#                                               uiOutput("year_data"),
#                                               radioButtons("dataType", label = "Show",inline = TRUE,
#                                                            choices = list("Import", 
#                                                                           "Export",
#                                                                           "Both"),
#                                                            selected = "Import"),
#                                               downloadButton('dlMap', 'Save map in A4 vector pdf')
#                                 )#,



                                
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

))