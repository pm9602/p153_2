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
                                        
                                        draggable = FALSE, height = "auto",
                                        
                                        br(),
                                        
                                        # show current dtg being presented
                                        
                                        h3(textOutput("dtg.text")),
                                        
                                        #slider input
                                        
                                        uiOutput("dtg.select"),
                                        
                                        uiOutput("proportion"),
                                        
                                        conditionalPanel(
                                          
                                          condition = "input.radio == 3 || input.radio == 4",
                                          
                                          sliderInput("slider.age", label = h5("Age Range (0-90)"), min = 0, max = 90, value = c(68, 90))
                                          
                                        ),
                                        
                                        conditionalPanel(
                                          
                                          condition = "input.radio == 5",
                                          
                                          selectInput("pred", label = h5("Select number of days to look forward"), 
                                                      choices = list("1" = 1, "2" = 2, "3" = 3,"4" = 4,"5" = 5), 
                                                      selected = 5)
                                          
                                        ),
                                        
                                        DT::dataTableOutput("table")
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

