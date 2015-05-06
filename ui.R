# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")

shinyUI(navbarPage("FAOSTAT Trade Data Explorer", id="nav",
                   
  tabPanel("Single-variable explorer",
          div(class="inner",
              
              tags$head(
                # Include our custom CSS
                includeCSS("styles.css"),
#                 # Hide the red error messages!!!
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",
                           ".outputRow{height:550px}"
                )
                
              ), 
              
  fluidRow(
    shiny::column(4, tags$h4("Hello world"), 
                  tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
    shiny::column(4, tags$br(),tags$br(),tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper, metus ac convallis vestibulum, elit arcu imperdiet neque, nec interdum quam dolor at libero.")),
    shiny::column(4, tags$br(),tags$br(),tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/logo200.png"))
    ),
  tags$h3("Select data"),
  fluidRow(
    shiny::column(4, uiOutput("domain")),
    shiny::column(4, uiOutput("item")),
    shiny::column(4, uiOutput("element"))
  ),
  tags$hr(),
  tags$h3("Time-series"),
  fluidRow(
    shiny::column(9, plotOutput("timeseries")),
    shiny::column(3, uiOutput("yearRange"), 
                     uiOutput("yRange"), 
                     downloadButton('dlTimeseries', 'Download plot'),
                     tags$br(),
                     downloadButton('dlDataTimeseries', 'Download data'))
    ),
  tags$hr(),
  tags$h3("Bar-plot"),
  fluidRow(
    shiny::column(9, plotOutput("bar")),
    shiny::column(3, uiOutput("yearBar"),
                     uiOutput("countryBar"), 
                      downloadButton('dlBar', 'Download plot'),
                      tags$br(),
                      downloadButton('dlDataBar', 'Download data'))
  ),
  tags$hr(),
  tags$h3("Map"),
  fluidRow(
    shiny::column(9, plotOutput("map")),
    shiny::column(3, uiOutput("yearMap"), 
                      downloadButton('dlMap', 'Download plot'),
                      tags$br(),
                      downloadButton('dlDataMap', 'Download data'))
  )
  )
),

tabPanel("Trade-matrix Explorer",
         
         
         div(class="inner",
             
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               
               
             ),
             
             tags$img(src="http://koti.kapsi.fi/~muuankarski/fao/visualisation/gif/maps.gif", width="1100px")
             
#              tags$h2("Select data"),
#              tags$h3("Variable X"),
#              fluidRow(
#                shiny::column(6, uiOutput("var_x_item")
#                ),
#                shiny::column(6, uiOutput("var_x_element")
#                )
#              ),
#              tags$h3("Variable Y"),
#              fluidRow(
#                shiny::column(6, uiOutput("var_y_item")
#                ),
#                shiny::column(6, uiOutput("var_y_element")
#                )
#              ),
#              tags$h3("Select Year"),
#              fluidRow(
#                shiny::column(12, uiOutput("bivar_year")
#                )
#              ),
#              
#              tabsetPanel(type= "tabs", position= "above",
#                          tabPanel("Single year plot", plotOutput("single_scatter", width="100%", height = "750px")),
#                          tabPanel("All years plot", plotOutput("multi_scatter", width="100%", height = "750px"))
#              )
             

         )
)

))