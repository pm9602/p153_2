  if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(xml2)) install.packages("xml2", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(leafpop)) install.packages("leafpop", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
  
########################################################################################################################################
# DATA MANAGEMENT 

' The aim of this refresh is to re organise how data is ingested, stored and handled within p153. Ingestion will be normalised as much as 
possible across the 5 different regions. It will be split out and stored in different data objects and will be managed through dplyr::joins
within the application itself. The data objects that need creating are:

UK Spatial Polygons Data Frame containing only codes and names for each region
UK demographics per region
UK Covid Confirmed Cases Per day
UK Compliance per region

'
# UK COVID Confirmed Cases Ingest

# England - manual xls input
# Scotland - manual xls input
# Wales - manual xls input
# NI - manual xls input
# Guernsey - manual xls input
# Isle of man - manual xls input
# Jersey - manual xls input

load_cases <- function(){
  
  load(file = "data/UK_cases.Rdata")
  #save(UK.cases, file = paste0("backup/",gsub(":","-",Sys.time()),"_UK_cases_backup.Rdata"))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ENGLAND DATA INPUT
  
  # load data from xls file
  eng.in <- readxl::read_xls("data/phe.xls") %>% as.data.frame
  
  # join UK.cases and cases.eng
  eng.df <- right_join(UK.cases[,c(1,2)],eng.in,by = "name")
  
  # detect any incomplete joins 
  eng.join_table <- sapply(eng.df$code, function(x) NA %in% x) %>% table
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NI, SCOTLAND, WALES, JERSEY, GUERNSEY, ISLE OF MAN DATA INPUT
  
  # load data from xls file
  ni.in <- readxl::read_xls("data/NI.xls") %>% as.data.frame
  
  # join UK.cases and ni.in
  ni.df <- right_join(UK.cases[,c(1,2)],ni.in,by = "name")
  
  # detect any incomplete joins 
  ni.join_table <- sapply(ni.df$code, function(x) NA %in% x) %>% table
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compare to historic reporting
  
  # comparison df - get rid of any NA values (to be dealt with manually)
  comp.df <- rbind(eng.df,ni.df)
  
  # record which records have not merged succesfully
  not_included <- is.na(comp.df$code) %>% which %>% comp.df$name[.]
  
  # record which figures are missing due to an input error
  missing_record <- is.na(comp.df$cases) %>% comp.df$name[.]
  
  # update comparison df with latest stored figures
  comp.df <- left_join(UK.cases[,c(1,ncol(UK.cases))],comp.df[c(1,3)], by = "code")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # UPDATE CASES RECORDS
  
  # if this condition is satisfied it means there are less than 100% exact matches between the historic data and the new data. If this is the case we create a new data frame
  if(((comp.df[,2] == comp.df[,3]) %>% sum(.)) < nrow(UK.cases)) {
    
    # any pairs that do not match as described by this condition !(comp.df[,2] == comp.df[,3]) get an updated TOI
    UK.cases$TOI[!(comp.df[,2] == comp.df[,3])] <- as.character(Sys.time())
    
    # if there has already been a data update today, it is over written. If not, we add a new column and give it todays date as a title
    if(colnames(UK.cases)[ncol(UK.cases)] == as.character(Sys.Date())) { 
      
      UK.cases[,ncol(UK.cases)] <- comp.df$cases
      
    } else{
      
      UK.cases$new <- comp.df$cases
      
      colnames(UK.cases)[ncol(UK.cases)] <- as.character(Sys.Date())
      
    }
    
    # save data frame back to file once edited
    save(UK.cases, file = "data/UK_cases.Rdata")
    
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  # ensure that UK.cases is numeric
  UK.cases[,ncol(UK.cases)] <- UK.cases[,ncol(UK.cases)] %>% as.numeric
  
  return(UK.cases)
  
}

#########################################################################################################################################

# create global variables function
assign_global_variables <- function(){
  # create global variabls for action button control
  assign("act1", act1 <- 0, envir = .GlobalEnv)
  assign("act2", act2 <- 0, envir = .GlobalEnv)
  assign("act3", act3 <- 0, envir = .GlobalEnv)
  assign("act4", act4 <- 0, envir = .GlobalEnv)
  assign("act5", act5 <- 0, envir = .GlobalEnv)
  assign("act6", act6 <- 0, envir = .GlobalEnv)
  
  # load spatial data frame containining boundaries and set as global variable
  bdys <- rgdal::readOGR("UK_detailed.geojson")
  assign("bdys",bdys, envir = .GlobalEnv)
  
  # load cases data as global variable
  UK.cases <- load_cases()
  assign("UK.cases",UK.cases, envir = .GlobalEnv)
  
  # LOAD DEMOGRAPHIC DATA AS GLOBAL VARIABLE
  load(file = "data/UK_demographics.Rdata")
  assign("UK.dem",UK.dem, envir = .GlobalEnv)
  
  #  LOAD GOOGLE COMMUNITY DATA
  
  gdata <- readxl::read_xls("data/gdata_analysis.xls") %>% as.data.frame
  
  # any data with 0 values changed to NA instead so its not displayed
  gdata[(gdata[,4] == 0),4] <- NA
  gdata[(gdata[,5] == 0),5] <- NA
  gdata[(gdata[,6] == 0),6] <- NA
  gdata[(gdata[,7] == 0),7] <- NA
  gdata[(gdata[,8] == 0),8] <- NA
  gdata[(gdata[,9] == 0),9] <- NA
  gdata[(gdata[,10] == 0),10] <- NA
  
  assign("gdata",gdata,envir = .GlobalEnv)
  
  }

server <- function(input, output){
 
  #################################################################################################################################
  #################################################################################################################################
  #---------------------------------------------------MAP TAB----------------------------------------------------------------------
  #################################################################################################################################
  #################################################################################################################################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # BUILD AND DISPLAY MODALS
  # create modal box
  showModal(modalDialog(title="DATA LOADING - PLEASE WAIT...","Please wait for map to appear before proceeding.",size="l",footer=NULL))
  

  # call function that creates global variable
  observe({
    
    assign_global_variables()
    
    removeModal()
    
  })
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # UI BUILD AND MANAGEMENT
  
  #--------------------
  # UI WIDGET BUILD
  #--------------------
  
  #-------------------------------------
  # Give MAP Tab a  title
  
  output$page_title <- renderText({
    
    req(!is.null(bselect()))
    
    title_list <- list(
      "1" = "Cases per 100000", 
      "2" = "Total Number of Cases",
      "3" = "Age Range as Percentage of Population",
      "4" = "Percentage Change in Cases Over Time",
      "5" = "Google Community Data"
      )
    
    title_list[bselect()] %>% as.character
    
  })
  #-------------------------------------
  
  
  #-----------------------------------------------------------------------------
  # Create UI objects for conditional panel 1 - Cases per 100000 and Total Cases
  
  # slider for selecting historic data
  output$dtg.select <- renderUI({
    req(exists("UK.cases"))
    
    sliderInput("dtg.server",
                label = NULL,
                min = 1,
                max = ncol(UK.cases) - 3,
                value = ncol(UK.cases) - 3,
                step = 1,
                animate = animationOptions(interval = 1000),
                width = 500)
    
  })
  
  # display date of information being displayed
  output$cases_text <- renderText({
    req(exists("UK.cases"))
    req(!is.null(input$dtg.server))
    
    paste0("Showing ", colnames(UK.cases)[input$dtg.server+3])
    
  })
  #-----------------------------------------------------------------------------
  # Create UI objects for conidional panel 2 - Age Range
  
  # display age range as text
  output$age_text <- renderText({
    
    req(!is.null(input$age.server))

    paste0("Showing percentage of total population between ages ", input$age.server[1], " and ", input$age.server[2])
    
  })
  
  # create slider for setting age proportion
  output$age.select <- renderUI({
    #req(UK.dem)
    
    sliderInput("age.server",
                label = NULL,
                min = 0,
                max = 90,
                value = c(68,90),
                step = 1,
                animate = animationOptions(interval = 1000),
                width = 500)
    
  })
  #-----------------------------------------------------------------------------
  # Create UI objects for conidional panel 3 - proportional change
  
  # display date range as text
  output$change_text <- renderText({
    req(exists("UK.cases"))
    req(!is.null(input$change.server))
    
    paste0("Showing percentage change between ", colnames(UK.cases)[input$change.server[1] + 3], " and ", colnames(UK.cases)[input$change.server[2] + 3])
    
  })
  
  # create slider for setting age proportion
  output$change.select <- renderUI({
    req(exists("UK.cases"))
    sliderInput("change.server",
                label = NULL,
                min = 1,
                max = ncol(UK.cases)-3,
                value = c(ncol(UK.cases)-8,ncol(UK.cases)-4),
                step = 1,
                animate = animationOptions(interval = 1000),
                width = 500)
    
  })
  #-----------------------------------------------------------------------------
  
  #--------------------
  # BUTTON MANAGEMENT
  #--------------------
  #-----------------------------------------------------------------------------
  # Not actually a UI function itself, it manages teh output from the action button panel
  bselect <- reactive({

    # on opening the program out doesn't actually exists, and thus needs a value
    if(exists("out") == FALSE) out <- 1
    if(input$act1 > max(act1)) out <- 1
    if(input$act2 > max(act2)) out <- 2
    if(input$act3 > max(act3)) out <- 3
    if(input$act4 > max(act4)) out <- 4
    if(input$act5 > max(act5)) out <- 5
    
    # if action button is pressed, overwrite global variable 
    if(input$act1 > 0){assign("act1", c(act1,input$act1), envir = .GlobalEnv)}
    if(input$act2 > 0){assign("act2", c(act2,input$act2), envir = .GlobalEnv)}
    if(input$act3 > 0){assign("act3", c(act3,input$act3), envir = .GlobalEnv)}
    if(input$act4 > 0){assign("act4", c(act4,input$act4), envir = .GlobalEnv)}
    if(input$act5 > 0){assign("act5", c(act5,input$act5), envir = .GlobalEnv)}
    
    out
    
  })
  
  # pass bselect variable to UI for use in conditional formatting
  output$id<-reactive({bselect()})
  #-----------------------------------------------------------------------------
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CREATE DATA FOR PASSING TO LEAFLET PROXY 
  
  # reactive data variable (changes depending on selection)
  data_reac <- reactive({
    
    # require these variables before continuing
    req(UK.cases)
    req(UK.dem)
    req(!is.null(input$dtg.server))
    req(!is.null(input$change.server))
    req(!is.null(input$gdata_select))
    
    # create temp cases file
    temp_cases <- left_join(bdys@data,UK.cases[c(1,(input$dtg.server+3))], by = "code")
    
    # temp join spdf, UK cases and total age
    temp.dem <- left_join(bdys@data,UK.dem[,c(1,3)], by = "code")
    
    # create temp gdata
    temp.gdata <- left_join(bdys@data, gdata[,c(1,as.numeric(input$gdata_select) + 3)], by = "code")
    
    
    #-----------------
    # CASES PER 100000
    #-----------------
    if(bselect() == 1){
      
        # return number of cases per 100000
        (100000 * temp_cases[,3] / temp.dem[,3]) %>% round(.,0)
      
      #-----------------
      # TOTAL CASES
      #-----------------  
      
      } else if(bselect() == 2){
      
        # return total number of cases
        temp_cases[,3]
      
      #-----------------
      # AGE PROPORTION
      #-----------------
        
      } else if (bselect() == 3){
        
        # percentage of population between input$age.server[1] and input$age.server[2]
        100 * ((UK.dem[,c((input$age.server[1]+4):(input$age.server[2]+4))] %>% rowSums) / UK.dem[,3]) %>% round(.,4)
        
      } else if(bselect() == 4){
        
        (100 * (UK.cases[,input$change.server[2] + 3] - UK.cases[,input$change.server[1] + 3]) / UK.cases[,input$change.server[1] + 3]) %>% round(.,2)
        
      } else if (bselect() == 5){
        
        (temp.gdata[,3] %>% round(.,4)) * 100
        
      }
    
    
    })
  
  # create popup for addPolygons function
  popup <- reactive({
    req(UK.cases)
    req(UK.dem)
    
    # temp join sp df and UK cases (only latest cases)
    temp.max.cases <- left_join(bdys@data,UK.cases[c(1,ncol(UK.cases))], by = "code")
    
    # temp join spdf, UK cases and total age
    temp.dem <- left_join(bdys@data,UK.dem[,c(1,3)], by = "code")
    
    paste(sep = "<br/>"
          
          ,paste0("<b>",temp.max.cases[,2],"</b>")
          
          ,""
          
          ,"COVID 19 DATA"
          
          ,paste0("Total Cases: ",temp.max.cases[,3])
          
          ,paste0("Cases per 100000: ", (100000 * temp.max.cases[,3] / temp.dem[,3]) %>% round(.,0))
          
          ,""
          
          ,"DEMOGRAPHIC DATA"
          
          ,paste0("Total Population: ", temp.dem[,3])

          )
  
    })
  
  # create colour palette
  col.pal <- reactive({
    
    # conditionally choose colours
    conditional.colour <- if(bselect() == 1 | bselect() == 2) {
      "Reds"
    } else if (bselect() == 3) {
        colorRampPalette(brewer.pal(8, "Blues"))(length(data_reac()))
    } else if (bselect() == 4){
        "Oranges"
    } else if (bselect() == 5){
      "BuPu"
    }
    
    colorBin(conditional.colour , domain = data_reac())
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CREATE MAP 
  
  output$mymap <- renderLeaflet({
    # need double click for adding icons
    leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>%
      
      setView(lng = 2, lat = 55, zoom = 5) %>%
      
      addTiles()
    
  })
  
  # ADD REGIONAL POLYGONS TO MAP
  observe({
    
    leafletProxy("mymap", data = bdys) %>%
      
      
      # clear old legends off map
      clearControls() %>%
      
      # add new legends
      addLegend(
        
        title = NULL,
        
        pal = col.pal(),
        
        values = data_reac(),
        
        "bottomleft",
        
        opacity = 1
        
      ) %>%
      
      
      # add spatial information
      addPolygons(
        # layer ID shouldnt really be needed... but it ensures that the polygons that have been placed on the map all have IDs that match with the spatial polygons object data frame
        layerId = rownames(bdys@data),
        
        popup = popup(),
        
        fillColor = col.pal()(data_reac()),
        
        # define outline features and opacity of shapes
        smoothFactor = 0.6,
        
        fillOpacity = 0.8,
        
        # line colour
        color = "black",
        
        weight = 0.5
        
        )
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CREATE TABLES
  
  # create data table for download
  output$table_cases <- renderDataTable({
    req(UK.cases)
    datatable(
      
      # dont display TOI
      UK.cases[,-3],
      
      rownames = FALSE,
      
    )
    
  })
  
  # create companion table
  
  output$table_companion <- renderDataTable({
    
    req(!is.null(input$gdata_select))
    
    temp.df <- data.frame(Region = bdys@data$name, Data = data_reac())
    
    # order from highest to lowest
    temp.df <- temp.df[order(-temp.df[,2]),]
    
    # conditional data column name 
    # create Gdata names in order to pass to data.column.name
    gdata.names <- paste0(colnames(gdata)[as.numeric(input$gdata_select) + 3],", %")

    
     data.column.name <- list(
      "1" = "Cases per 100000", 
      "2" = "Total Cases",
      "3" = "%",
      "4" = "%",
      "5" = gdata.names
      )
     
     # assign correct name to table
     colnames(temp.df)[2] <- as.character(data.column.name[bselect()])
     
    datatable(
      
      # dont display TOI
      temp.df,
      
      rownames = FALSE,
      
    )
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CREATE DOWNLOAD FUNCTIONS
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste0("Data_gathered_and_aggregated_by_Paddy_Mullany_as_at_",Sys.time() %>% gsub(":","-",.) %>% gsub(" ","_",.), ".csv")
      
    },
    
    content = function(file) {
      req(UK.cases)
      
      write.csv(UK.cases[,-3], file, row.names = FALSE)
      
    }
  )
  
}






