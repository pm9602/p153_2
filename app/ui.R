if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")

ui <- navbarPage(theme = shinytheme("cosmo"), collapsible = TRUE,
                 "P153.1 UI 0.1",
                 
                 # Render the map panel
                 tabPanel("Map",
                          
                          div(class="outer",
                              
                              tags$head(includeCSS("style.css")),
                              
                              leafletOutput("mymap", width="100%", height="100%")
                          ),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                          
                                        top = 80, right = 20, width = 550, fixed=F,
                                        
                                        draggable = TRUE, height = "auto",

                                        # create action buttons display
                                        
                                        div(style = "position:absolute;left:2.5em;position:absolute;top:2em;", 
                                            actionButton('act1', 'Cases per 100000', width = 220)
                                        ),
                                        
                                        div(style = "position:absolute;right:2.5em;position:absolute;top:2em;", 
                                            actionButton('act2', 'Total Cases', width = 220)
                                        ),
                                        
                                        div(style = "position:absolute;left:2.5em;position:absolute;top:6.5em;",
                                            actionButton('act3', 'Age Range', width = 220)
                                        ),
                                        
                                        div(style = "position:absolute;right:2.5em;position:absolute;top:6.5em;",
                                            actionButton('act4', 'Cases to Age Range Ratio', width = 220)
                                        ),
                                        
                                        div(style = "position:absolute;left:2.5em;position:absolute;top:11em;",
                                            actionButton('act5', 'Predictive Analysis', width = 220)
                                        ),
                                        
                                        div(style = "position:absolute;right:2.5em;position:absolute;top:11em;",
                                            actionButton('act6', '3 Day % Increase', width = 220)
                                        ),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        br(),
                                        
                                        # show current dtg being presented
                                        
                                        #verbatimTextOutput("id"),
                                        
                                        h3(textOutput("dtg.text")),
                                        
                                        #slider input
                                         
                                        uiOutput("dtg.select"),
                                        
                                        #uiOutput("proportion"),
                                        
                                        conditionalPanel(
                                          
                                          condition = "output.id == 3 || output.id == 4",
                                          
                                          sliderInput("slider.age", label = h5("Age Range (0-90)"), min = 0, max = 90, value = c(68, 90), width = 500)
                                          
                                        ),
                                        
                                        conditionalPanel(
                                          
                                          condition = "output.id == 5",
                                          
                                          selectInput("pred", label = h5("Select number of days to look forward"), 
                                                      choices = list("1" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5), 
                                                      selected = 5,
                                                      width = 500)
                                          
                                        ),
                                        
                                        DT::dataTableOutput("table"),
                                        # denotes which layer is showing. MUST BE DISPLAYED FOR LAYER FILTER FUNCION TO WORK. I've hidden down here for the time being whilst I work out how to get rid of it.
                                        verbatimTextOutput("id")
                          )
                          
                 ),
                 tabPanel("Data",
                          
                          fluidRow(
                            
                            downloadButton("downloadData", "Download")
                            
                          ),
                          
                          br(),
                          
                          fluidRow(
                            
                            DT::dataTableOutput("table.out")  
                          
                            )
                          

                 )
                 
)

