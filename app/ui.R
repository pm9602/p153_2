if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")

library(shinyWidgets)


ui <- navbarPage(theme = shinytheme("cosmo"), collapsible = TRUE,
                 "P153.1 UI 0.1"

                
######################################################################################################################################################                 
######################################################################################################################################################
#----------------------------------------------------------------MAP TAB------------------------------------------------------------------------------
######################################################################################################################################################
######################################################################################################################################################


                 # Render the map panel
                 ,tabPanel("Map"
                          
                          ,div(class="outer"
                             
                            #--------------------------------LOAD DESIGN FEATURES HERE---------------------------------- 
                            # LOAD CSS
                            ,tags$head(includeCSS("style.css"))
                            
                            
                            # WIDGET DESIGN FEATURES
                            ,chooseSliderSkin("Modern")
                            
                            ,setSliderColor(c("OrangeRed","Crimson","DarkSlateBlue"),c(1,2,3))
                               
                            ,leafletOutput("mymap", width="100%", height="100%")
                          
                            )

                          
                          # create companian panel
                          ,absolutePanel(id = "controls", class = "panel panel-default"
                          
                                        ,top = 80, right = 20, width = 350, fixed=F
                                        
                                        ,draggable = TRUE, height = "auto"

                                        #---------------------------------create action buttons display----------------------------------------------
                                        
                                        ,h4(tags$b(textOutput("page_title")))
                                        ,tags$hr()
                                        # deliberately hide id output as its needed for UI control but does not provide value to use
                                        
                                        ,div(style = "position:absolute;left:.8em;position:absolute;top:5.5em;",
                                            textOutput("id")
                                        )
                                        
                                        ,div(style = "position:absolute;left:.8em;position:absolute;top:5.5em;", 
                                            actionButton('act1', 'Cases per 100000', width = 154, style='padding:4px;')
                                        ),
                                        
                                        div(style = "position:absolute;right:.8em;position:absolute;top:5.5em;", 
                                            actionButton('act2', 'Total Cases', width = 154, style='padding:4px;')
                                        ),
                                        
                                        div(style = "position:absolute;left:.8em;position:absolute;top:8.5em;",
                                            actionButton('act3', 'Age Range', width = 154, style='padding:4px;')
                                        ),
                                        
                                        div(style = "position:absolute;right:.8em;position:absolute;top:8.5em;",
                                            actionButton('act4', 'Change in Cases', width = 154, style='padding:4px;')
                                        ),
                                        
                                        div(style = "position:absolute;left:.8em;position:absolute;top:11.5em;",
                                            actionButton('act5', 'Google Data', width = 154, style='padding:4px;')
                                        )
                                        
                                        
                                      
                                        ,br(),br(),br(),br(),br(),br(), br()
                                       
                                        
                                        
                                        ,h4("Control Panel")

                                        ,tags$hr()
                                        
                                        #########################################################################################
                                        # CASES CONDITIONAL PANEL 1- Cases per 100000 and Total Cases (output.id == 1 or 2)
                                        ,conditionalPanel(
                                          
                                          condition = "output.id == 1 || output.id == 2"
                                          
                                          ,h4(textOutput("cases_text"))
                                          ,br()
                                          ,uiOutput("dtg.select")
                                          
                                        )
                                        
                                        #########################################################################################
                                        # CASES CONDITIONAL PANEL 2- Age Range (output.id == 3)
                                        ,conditionalPanel(
                                          
                                          condition = "output.id == 3"
                                          
                                          ,h4(textOutput("age_text"))
                                          ,br()
                                          ,uiOutput("age.select")
                                          
                                        )
                                        #########################################################################################
                                        
                                        # CASES CONDITIONAL PANEL 3- Age Range (output.id == 4)
                                        ,conditionalPanel(
                                          
                                          condition = "output.id == 4"
                                          
                                          ,h4(textOutput("change_text"))
                                          ,br()
                                          ,uiOutput("change.select")
                                          
                                        )
                                        
                                        #########################################################################################
                                        
                                        # CASES CONDITIONAL PANEL 4- Google Commnity Data (output.id == 5)
                                        ,conditionalPanel(
                                          
                                          condition = "output.id == 5"
                                          
#                                          ,h4(textOutput("change_text"))
                                           ,br()
#                                          ,uiOutput("change.select")
                                           ,  selectInput("gdata_select",
                                                           label = NULL,
                                                           choices = list("Retail and Recreation" = 1
                                                                          ,"Grocery and Pharmacy" = 2
                                                                          ,"Parks" = 3
                                                                          ,"Transit Stations" = 4
                                                                          ,"Workplaces" = 5
                                                                          ,"Residential" = 6
                                                                          ,"Average Change in Negative Beahviour" = 7), 
                                                           selected = 1
                                                          )
                                            , tags$p("This is Google Community Data from the 30th March.
                                                     It shows percentage change in a local population visiting
                                                     one of the 6 area types listed. Overal negative behaviour is
                                                     taken as an average of all behaviour other than staying at
                                                     home. The report doesn't map eactly to all regions on the map,
                                                      so an average is taken. Follow link for actual report: ")

                                            , tags$a(href = "https://www.gstatic.com/covid19/mobility/2020-03-29_GB_Mobility_Report_en.pdf", "UK Google Community Mobility Report")
                                          
                                        )

                                        # show companion table 
                                        ,checkboxInput("table_check", label = "Show Table", value = FALSE)
                                        
                                        # CASES CONDITIONAL PANEL 4- companion table
                                        #########################################################################################
                                        
                                        ,conditionalPanel(
                                          
                                          condition = "input.table_check == 1"
                                          
                                          ,DT::dataTableOutput("table_companion")
                                          
                                        )
                                        
                                        #########################################################################################
                                        

                                
                          )
                                        
                 )
          

######################################################################################################################################################                 
######################################################################################################################################################
#----------------------------------------------------------------DATA TAB-----------------------------------------------------------------------------
######################################################################################################################################################
######################################################################################################################################################

                    ,tabPanel("Data"
                             
                             ,downloadButton("downloadData", "Download CSV")
                             
                             ,br(),br()
                             
                             ,DT::dataTableOutput("table_cases")
                             
                    )
)

