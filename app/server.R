#load libraries
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(spdplyr)) install.packages("spdplyr", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(raster)) install.packages("raster", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(mime)) install.packages("mime", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(rmapshaper)) install.packages("rmapshaper", repos = "http://cran.us.r-project.org")
if(!require(maptools)) install.packages("maptools", repos = "http://cran.us.r-project.org")
if(!require(arsenal)) install.packages("arsenal", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

div.mul <- function(x,y)return(round(100000 * x/y,digits = 0))

ICU.func <- function(cases){
  
  if (cases < 69176) {
    
    round((1.702 * cases)^0.6953,digits = 0)
    
  } else if (cases >= 69176){
    
    m <- (3732-3396)/(86498 - 69176)
    
    b <- 3356 - 69176*m
    
    round(m * cases + b,digits = 0)
    
  }
  
} 


# SPANISH DATA
# enter days, returns number of cases
es.func1 <- function(days) 0.00000000000003319  * days ^ 8.91 - 1.104

# enter cases, returns which day you should be on
es.func2 <- function(cases) ((cases + 1.104)/0.00000000000003319) ^(1/8.91)

# UK DATA
# enter days, returns number of cases
uk.func1 <- function(days) 0.00000000000000002052  * days ^ 10.22 + 0.003966 

# enter cases, returns which day you should be on
uk.func2 <- function(cases) ((cases - 0.003966)/0.00000000000000002052)^(1/10.22)

# load.data function loads historic data, pulls in data from all sources, compares the two and creates a global spatial polygons object with any new data.
load.data <- function(){
  ##########################################################################################################################
  #-------------------------------------------------LOAD HEALTH DATA--------------------------------------------------------
  ##########################################################################################################################
  
  # LOAD LOCAL DATA OBJECT
  load(file = "UK.Rdata")

  #*************************************************************************************************************************
  # PUBLIC HEALTH ENGLAND
  #*************************************************************************************************************************

  #########################################
  # TEMP XLS LOAD WHILST API FIXED
  #########################################
  
  phe.df <- readxl::read_xls("phe.xls")
  
  phe.df$cases <- phe.df$cases %>% as.numeric()

  phe.df <- merge(UK.i[,c(1,2)],phe.df,by = "name")
  
  phe.df <- phe.df[,c(2,1,3)]
  
  # dtg as character for df entry
  df.name <- Sys.time() %>% as.character()
  
  #add TOI column
  phe.df <- cbind(phe.df,rep(df.name,nrow(phe.df)))
  
  colnames(phe.df) <- c("code","name",df.name,"TOI") 
  
  # check if phe has any new data and assign new TOI to entire data set
  phe.diff <- merge(UK.i$code %in% phe.df$code %>% UK.i[.,c(1,ncol(UK.i))],phe.df[,c(1,3)], by = "code")
  
  phe.diff <- (phe.diff[,2] - phe.diff[,3]) %>% sum
  
  if(phe.diff == 0){
    
    phe.df$TOI <- UK.i$code %in% phe.df$code %>% UK.i$TOI[.]
    
  }
  
  # ADD SOURCE COLUMN 
  
  eng.url <- "https://www.arcgis.com/apps/opsdashboard/index.html#/f94c3c90da5b4e9f9a0b19484dd4bb14"
  
  phe.df <- cbind(phe.df,
                  
                  rep(eng.url,nrow(phe.df)),
                  
                  rep("Manually copied from PHE Dashboard. Dashboard is check for changes at 0730, 1200 and 1830 every day",nrow(phe.df))
                  
                  )
  
  colnames(phe.df)[5:6] <- c("source","comment") 
  
  #*************************************************************************************************************************
  # NHS SCOTLAND
  #*************************************************************************************************************************
  
  url <- "https://www.gov.scot/coronavirus-covid-19/"
  
  err.catch <- tryCatch(read_html(url), error = function(e){"no_url"}) 
  
  if(err.catch != "no_url") {
    
    nhs.scot <- read_html(url)
    
    nhs.scot.data <- html_nodes(nhs.scot, "td") %>%
      html_text() %>%
      {gsub("\n","",.)} %>%
      {gsub("\r","",.)} %>%
      matrix(ncol = 2, byrow = TRUE) %>% as.data.frame()
    
    colnames(nhs.scot.data) <- c("name",df.name)
    
    # there have been different types of whitespace being returned, causing the merge function to drop values. This has only been seen in scots data
    nhs.scot.data$name <- nhs.scot.data$name %>% {gsub(intToUtf8(160),intToUtf8(32),.)}
    nhs.scot.data$name <- nhs.scot.data$name %>% as.character() %>% str_trim(.,side = "both") %>% {gsub("  "," ",.)}
    
    # get rid of non numeric values in numbers
    nhs.scot.data[,2] <- nhs.scot.data[,2] %>% {gsub("[^0-9.-]", "", .)} 
    
    nhs.scot.df <- merge(UK.i[,c(1:2)],nhs.scot.data,by = "name")
    
    nhs.scot.df <- nhs.scot.df[,c(2,1,3)]
    # returns corvid cases as numbers, not factors
    nhs.scot.df[,3] <- nhs.scot.df[,3] %>% as.character %>% as.numeric
    
  } else {
    #we will use old scottish data if we have to...
    nhs.scot.df  <- UK.i$code %>% {grep("S",.)} %>% UK.i[.,c(2,1,ncol(UK.i))]
    
    colnames(nhs.scot.df)[3] <- df.name
    
    nhs.scot.df <- nhs.scot.df[,c(2,1,3)]
  
  }
  
  #add TOI column
  nhs.scot.df <- cbind(nhs.scot.df,rep(df.name,nrow(nhs.scot.df)))
  
  colnames(nhs.scot.df)[4] <- "TOI"
  
  # check if phe has any new data and assign new TOI to entire data set
  scot.diff <- merge(UK.i$code %in% nhs.scot.df$code %>% UK.i[.,c(1,ncol(UK.i))],nhs.scot.df[,c(1,3)], by = "code")
  
  scot.diff <- (scot.diff[,2] - scot.diff[,3]) %>% sum
  
  if(scot.diff == 0){
  
      nhs.scot.df$TOI <- UK.i$code %in% nhs.scot.df$code %>% UK.i$TOI[.]
  
      }
  
  # ADD SOURCE COLUMN 
  
  nhs.scot.df <- cbind(nhs.scot.df,
                       
                       rep(url,nrow(nhs.scot.df)),
                       
                       rep("Scraped from NHS Scotland website on opening P153 App. NHS Scotland update every day at 1400",nrow(nhs.scot.df))
                       
                       )
                       
  
  colnames(nhs.scot.df)[5:6] <- c("source","comment") 
                       


  #*************************************************************************************************************************
  # NHS Wales
  #*************************************************************************************************************************
  
  url.wales <- "https://covid19-phwstatement.nhs.wales/"
  
  err.catch2 <- tryCatch(read_html(url.wales), error = function(e){"no_url"}) 
  
  if(err.catch2 != "no_url"){
  
      nhs.wales <- read_html(url.wales)
    
    # clean up that sweet welsh data
    
    nhs.wales.data <- html_nodes(nhs.wales, "td") %>%
      
      html_text() %>%
      
      {gsub("\n","",.)} %>%
      
      {gsub("\r","",.)} %>%
      
      matrix(ncol = 3, byrow = TRUE) %>%
      
      as.data.frame()
    
    # get rid of double spacing and spacing either side of words
    nhs.wales.data$V1 <- nhs.wales.data$V1 %>% as.character() %>% str_trim(.,side = "both") %>% {gsub("  "," ",.)}

    # we are dropping anything that isnt being reported as one of the 7 nhs regions of wales.
    nhs.wales.data.trim <- nhs.wales.data$V1 %>% {gsub(" ","",.)} %in% (UK.i$name %>% {gsub(" ","",.)}) %>% nhs.wales.data[.,]
    
    # get rid of non numeric characters in data
    nhs.wales.data.trim[,3] <- nhs.wales.data.trim[,3] %>% {gsub("[^0-9.-]", "", .)}
    
    # merge the core data codes with the newly pulled data (which doesnt come with codes.... >:[ ). Re order for happines and joy (and so we can merge it to the rest of the data)
    names(nhs.wales.data.trim) <- c("name","new",df.name) 
    
    nhs.wales.df <- merge(UK.i[,c(1:2)],nhs.wales.data.trim[,c(1,3)],by = "name")
    
    nhs.wales.df <- nhs.wales.df[,c(2,1,3)]
    
    nhs.wales.df <- nhs.wales.df[!(nhs.wales.df$code %>% duplicated),]
    # convert corvid cases to numeric
    nhs.wales.df[,3] <- nhs.wales.df[,3] %>% as.character %>% as.numeric

  } else {
    #we will use old welse data if we have to...
    nhs.wales.df  <- UK.i$code %>% {grep("W",.)} %>% UK.i[.,c(2,1,ncol(UK.i))]
    
    colnames(nhs.wales.df)[3] <- df.name
    
    nhs.wales.df <- nhs.wales.df[,c(2,1,3)]
  }

  #add TOI column
  nhs.wales.df <- cbind(nhs.wales.df,rep(df.name,nrow(nhs.wales.df)))
  
  colnames(nhs.wales.df)[4] <- "TOI"
  
  # check if phe has any new data and assign new TOI to entire data set
  wales.diff <- merge(UK.i$code %in% nhs.wales.df$code %>% UK.i[.,c(1,ncol(UK.i))],nhs.wales.df[,c(1,3)], by = "code")
  
  wales.diff <- (wales.diff[,2] - wales.diff[,3]) %>% sum
  
  if(wales.diff == 0){
    
    nhs.wales.df$TOI <- UK.i$code %in% nhs.wales.df$code %>% UK.i$TOI[.]
    
  }
  
  # ADD SOURCE COLUMN 
  
  nhs.wales.df <- cbind(nhs.wales.df,
                       
                       rep(url.wales,nrow(nhs.wales.df)),
                       
                       rep("Scraped from NHS Wales website on opening P153 App. NHS Wales update once a day, though when they update varies day to day.",nrow(nhs.wales.df))
                       
  )
  
  colnames(nhs.wales.df)[5:6] <- c("source","comment") 


  #*************************************************************************************************************************
  # Northern Ireland
  #*************************************************************************************************************************
  # we have to fat finger NIs data in atm

  nhs.ni.df <- readxl::read_xls("NI.xls")
  
  # add TOI data
  nhs.ni.df <- cbind(nhs.ni.df,rep(df.name,nrow(nhs.ni.df)))
  
  nhs.ni.df <- nhs.ni.df[,c(1:3,6,4,5)]
  
  colnames(nhs.ni.df) <- colnames(nhs.scot.df)
  
  nhs.ni.df[,6] <- rep("NHS Ireland updates total figures once a day. County figures are only updating every few days.",nrow(nhs.ni.df))
  
  #*************************************************************************************************************************
  # Isle of man
  #*************************************************************************************************************************
  #colnames(nhs.scot.df)
  url.im <- "https://covid19.gov.im/general-information/latest-updates/"
  
  err.catch.im <- tryCatch(read_html(url.im), error = function(e){"no_url"}) 
  
  if(err.catch.im != "no_url"){
    
    nhs.im <- read_html(url.im)
    
    # clean up that sweet isle of man data
    
    nhs.im.data <- html_nodes(nhs.im, "td") %>%
      
      html_text() %>%
      
      {gsub("\n","",.)} %>%
      
      {gsub("\r","",.)}
    
    # find start and end of string containing cases numeric
    s.s <- str_locate_all(pattern = "Confirmed cases:",nhs.im.data[1])
    s.e <- str_locate_all(pattern = "Awaiting results:",nhs.im.data[1])
    
    # extract cases number from nhs.im.data[1], strip out white spaces and convert to numeric
    im.cases <- substr(nhs.im.data[1],s.s[[1]][2]+1,s.e[[1]][1]-1) %>% str_trim(.,side = "both") %>% {gsub("  "," ",.)} %>% as.numeric
    
    # create Isle of man data frame
    im.df <- data.frame(
      code = "IM1",
      name = "Isle of Man",#UK.i$code %>% {grep("IM",.)} %>% UK.i[.,c(1,2)],
      cases = im.cases,
      TOI = df.name,
      source = "https://covid19.gov.im/general-information/latest-updates/",
      "Scraped from Isle of Man website on app opening.")

    } else {
    #we will use old data if we have to...
      im.df <- data.frame(
        code = "IM1",
        name = "Isle of Man",#UK.i$code %>% {grep("IM",.)} %>% UK.i[.,c(1,2)],
        cases = UK.i$code %>% {grep("IM1",.)} %>% UK.i[.,ncol(UK.i)], # this line pulls the most recent data from UK.i
        TOI = df.name,
        source = "https://covid19.gov.im/general-information/latest-updates/",
        "Scraped from Isle of Man local government tracker on app opening.")
  }
  
    
  colnames(im.df) <- colnames(nhs.scot.df)
  
  #*************************************************************************************************************************
  # Jersey
  #*************************************************************************************************************************
  url.je <- "https://www.gov.je/Health/Coronavirus/Pages/CoronavirusCases.aspx"
  
  jey.cases <- 81
  
  #if(jey.cases != UK.i$code %>% {grep("JEY1",.)} %>% UK.i[.,ncol(UK.i)]) jey.cases else UK.i$code %>% {grep("JEY1",.)} %>% UK.i[.,ncol(UK.i)]
  
  jey.df <- data.frame(
    code = "JEY1",
    name = "Jersey",
    cases = jey.cases,#UK.i$code %>% {grep("IM1",.)} %>% UK.i[.,ncol(UK.i)], # this line pulls the most recent data from UK.i
    TOI = df.name,
    source = url.je,
    "Manually entered every day from source.")
  
  colnames(jey.df) <- colnames(nhs.scot.df)
  
  #*************************************************************************************************************************
  # Guernsey
  #*************************************************************************************************************************
  url.ge <- "https://www.gov.gg/covid19testresults"
  
  err.catch.ge <- tryCatch(read_html(url.ge), error = function(e){"no_url"}) 
  
  if(err.catch.im != "no_url"){
    
    nhs.ge <- read_html(url.ge)
    
    # clean up that sweet isle of man data
    
    nhs.ge.data <- html_nodes(nhs.ge, "td") %>%
      
      html_text() %>%
      
      {gsub("\n","",.)} %>%
      
      {gsub("\r","",.)} %>% matrix(.,nrow = 2, byrow = T)
    
    ggy.cases <- nhs.ge.data[2,2] %>% as.numeric()
    
    # create Isle of man data frame
    ggy.df <- data.frame(
      code = "GGY1",
      name = "Guernsey",#UK.i$code %>% {grep("IM",.)} %>% UK.i[.,c(1,2)],
      cases = ggy.cases,
      TOI = df.name,
      source = url.ge,
      comment = "Scraped from Guernsey local government website on app opening.")
    
  } else {
    #we will use old data if we have to...
    ggy.df <- data.frame(
      code = "GGY1",
      name = "Guernsey",
      cases = UK.i$code %>% {grep("GGY1",.)} %>% UK.i[.,ncol(UK.i)], # this line pulls the most recent data from UK.i
      TOI = df.name,
      source = url.ge,
      comment = "Scraped from Guernsey local government website on app opening.")
  }
  
  colnames(ggy.df) <- colnames(nhs.scot.df)
  
  #--------------------------
  # Combine data
  #--------------------------
  # combine all nations corvid 19 data and look for differences in what we currently have
  comp.df <- rbind(phe.df,
                   nhs.scot.df,
                   nhs.wales.df,
                   nhs.ni.df,
                   im.df,
                   jey.df,
                   ggy.df)
  
  comp.merge <- merge(comp.df[,c(1,3)],UK.i[,c(1,ncol(UK.i))], all = T)
  
  #colnames(comp.merge)[3] <- colnames(comp.merge)[3] #%>% {as.Date(.)} %>% as.character
  
  comp.col <- (comp.merge[,3] %>% as.character %>% as.numeric())- (comp.merge[,2] %>% as.character %>% as.numeric())
  
  cc <- table(comp.col) # convert to table in order sum total number of 0 values

  # if there are any discrepencies between columns add new column to UK.i, save and reopen UK.i
  if(cc[names(cc) == 0] != nrow(UK.i))
  {
    
    UK.i <- merge(UK.i,comp.df[,c(1,3)],by = "code", all = T)
    
    # if the data update was from today, we delete previous columns from today
    if(as.Date(colnames(UK.i)[ncol(UK.i)]) == as.Date(colnames(UK.i)[ncol(UK.i)-1])){
      
      UK.i <- UK.i[,-(ncol(UK.i)-1)]
      
    }
    
    colnames(UK.i)[ncol(UK.i)] <- as.Date(colnames(UK.i)[ncol(UK.i)]) %>% as.character
    
    # if there are NA values we will use old values
    NA_val <-UK.i[,ncol(UK.i)] %>% is.na
    
    UK.i[NA_val,ncol(UK.i)] <- UK.i[NA_val,ncol(UK.i)-1] 
    
    UK.i$TOI <- comp.df$TOI
    
    UK.i <- UK.i[!(UK.i$code %>% duplicated()),]
    
    save(UK.i, file = "UK.Rdata")
    
    #save back up
    save(UK.i, file = paste0("backup/",gsub(":","-",Sys.time()),"_UK_backup.Rdata"))
    
    rm(UK.i)
    
    load(file = "UK.Rdata")
  }
  
##########################################################################################################################
#-------------------------------------------------------LOAD GEOGRAPHIC DATA----------------------------------------------
##########################################################################################################################
  set.a <- rgdal::readOGR("UK.geojson") 

##########################################################################################################################
#-------------------------------------------COMBINE GEOGRAPHIC AND HEALTH DATA-------------------------------------------
##########################################################################################################################
  
  #set.a will be my extant set of divisions for the UK. This is currently missing isle of man and guernsey. set.t will be the data I have for CORVID
  set.t <- set.a
  
  # set.a has plenty of overlaps, only show polygons with codes that are found in UK.i, and then merge them
  set.t <- set.t[set.a@data$code %in% UK.i$code,]
  
  # attach census data and historical case values to set.t
  set.t <- merge(set.t, UK.i[,c(1,3:ncol(UK.i))], by = "code")
  
  # attach url and comment data to set.t
  set.t <- merge(set.t,comp.df[,c(1,5,6)], by = "code")
  
  # re order set.t@data
  set.t@data <- set.t@data[,
                           c(
                             1:3, # first three columns stay in the same place
                             ncol(set.t@data)-1, # the last two columns get moved to pos 4 and 5
                             ncol(set.t@data),
                             4:(ncol(set.t@data)-2) # the rest of the data stays in the same place
                           )
                           ] 
  
  
  # these offsets are for the plotting functions later os1:os2 == 0:90 age
  os1 <- 7
  os2 <- 97
  
  assign("os1", os1, envir = .GlobalEnv)
  assign("os2", os2, envir = .GlobalEnv)
  
  
  # create global variabls for action button control
  assign("act1", act1 <- 0, envir = .GlobalEnv)
  assign("act2", act2 <- 0, envir = .GlobalEnv)
  assign("act3", act3 <- 0, envir = .GlobalEnv)
  assign("act4", act4 <- 0, envir = .GlobalEnv)
  assign("act5", act5 <- 0, envir = .GlobalEnv)
  assign("act6", act6 <- 0, envir = .GlobalEnv)
  
  
  assign("set.t", set.t, envir = .GlobalEnv)

  return()
  
}

#########################################################################################################################
#------------------------------------------------SERVER-----------------------------------------------------------------
#########################################################################################################################

server <- function(input, output){
  
  ####################################################################################################
  ####################################################################################################  
  # LOAD AND CREATE DATA
  
  # create modal box
  showModal(modalDialog(title="DATA LOADING - PLEASE WAIT...","Please wait for map to appear before proceeding.",size="l",footer=NULL))
  
  # load data - call function on time that creates a global variabl used by all. Asks for more data every hour
  autoInvalidate <- reactiveTimer(3600000)
  
  # call function that creates global variable
  observe({
    
    autoInvalidate()
    load.data()
  
    })
  
  # GET RID OF DATA LOADING MODAL, SHOW NEW MODAL GIVING DETAILS TO CUSTOMER
  observe({
    
    req(data.in)
    print("I got this far")
    removeModal()
    
    showModal(modalDialog(
    
        title = tags$b("P153 CORVID TRACKER"),
      
        "P153 tracks data from Scotland, England, Wales and Northern Ireland health services. The demographic data is pulled from the Office of National Statistics. The data is either scraped from the websites or manually entered. Please contact Paddy Mullany on" ,
        tags$b("patrick.a.m2020@gmail.com"),
        " if there are any issues, if you would like new features adding or if you would like to see the methodology behind the predictive analysis.",
        br(),
        br(),
        tags$b("Recent Updates"),br(),
        "- Historic data added and rationalised to day by day information",br(),
        "- Predictive analysis modelled on European case growth and Italian ICU bed usage",br(),
        "- Percentage change over 3 days added. Note that some areas (such as NI) will show large percentage increases. This is appears to be due to reporting standards changing, not sudden increases in cases"
        ))
  
    })

  #   define data.in
  data.in <- reactive({
    
    set.t
    
  })
  
  # create age.r dataframe. This contains some reactive calculations related to age demographics 
  age.df <- reactive({
    
    req(!is.null(input$slider.age))
    
    age.r <- data.frame(set.t@data[,c(1:3)],
                        
                        colnames(data.in()@data)[c(os1:os2)] %>% as.numeric() %>%
                          
                          # compare slider.age to all columns, return columns within range
                          {. <= input$slider.age[2] &. >= input$slider.age[1]} %>%
                          
                          # add (os1-1) extra false positions to account for non age positions in data.in
                          append(rep(FALSE,(os1-1)),.) %>%
                          
                          # pass true false vector to data.in@data
                          data.in()@data[,.] %>%
                          
                          # return data as percentage of local population
                          rowSums(.)*100/set.t@data[,(os1-1)],
                        
                        colnames(data.in()@data)[c(os1:os2)] %>% as.numeric() %>%
                          
                          # compare slider.age to all columns, return columns within range
                          {. <= input$slider.age[2] &. >= input$slider.age[1]} %>%
                          
                          # add (os1-1) extra false positions to account for non age positions in data.in
                          append(rep(FALSE,(os1-1)),.) %>%
                          
                          # pass true false vector to data.in@data
                          data.in()@data[,.] 
                        
    )
    
    
    # calulcate proportion of cases to age range  
    age.r[,5] <- round(sum(data.in()@data[,input$dtg.server+os2])/age.r[,5],0)
    
    age.r[,4] <- round(age.r[,4], digits = 2)
    
    age.r
    
  })
  
  # create pred.df for looking at potential cases
  pred.df <- reactive({
    
    req(!is.null(input$pred))

    data.frame(
      
      name = data.in()@data[,2],
      
      # cases per 10000 using spanish projectios in x number of days time
      UK_cases_pred = data.in()@data[,ncol(data.in()@data)] %>% div.mul(.,data.in()@data[,(os1-1)]) %>% {uk.func2(.)} %>% {uk.func1(. + as.numeric(input$pred))} %>% round(.,digits = 0) %>% as.numeric(),
      
      # cases per 10000 using uk projections in x number of days time
      Worst_cases_pred = data.in()@data[,ncol(data.in()@data)] %>% div.mul(.,data.in()@data[,(os1-1)]) %>% {es.func2(.)} %>% {es.func1(. + as.numeric(input$pred))} %>% round(.,digits = 0) %>% as.numeric(),
      
      # beds total using uk projections in x number of days time
      UK_bed_pred = data.in()@data[,ncol(data.in()@data)] %>% {uk.func2(.)} %>% {uk.func1(. + as.numeric(input$pred))} %>% ICU.func(.),
      
      # beds totl using spanish projections in x number of days time
      Worst_bed_pred = data.in()@data[,ncol(data.in()@data)] %>% {es.func2(.)} %>% {es.func1(. + as.numeric(input$pred))} %>% ICU.func(.)
      
      
      )
    
    
  })
  
  # create a reactive object showing percentage increase over a three day period
  inc.df <- reactive({
    # create dataframe by working out difference over 3 day period as percentage. Some of these values will be infinite as we dont have 3 days of values for everything
    inc.df <- data.frame(name = data.in()@data[,2],
                         increase = 100 * (data.in()@data[,ncol(data.in()@data)] - data.in()@data[,ncol(data.in()@data)-3])/data.in()@data[,ncol(data.in()@data)-3] 
    )
    
    # all infinite values are changed to 0
    inc.df  <- inc.df %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
    
    # all percentage values are rounded to 2 decimal places
    inc.df[,2] <- round(inc.df[,2],digits = 2)
    
    inc.df
  })
    
    
  ####################################################################################################
  ####################################################################################################  
  # BUILD UI FUNCTIONS FOR PASSING TO UI

  # slider for selecting historic data
  output$dtg.select <- renderUI({
    
    sliderInput("dtg.server",
                label = h4("View historical data"),
                min = 1,
                max = ncol(data.in()@data)-os2,
                value = ncol(data.in()@data)-os2,
                step = 1,
                animate = TRUE,
                width = 500)
    
    })
  
  # radio selector for altering the polygons that are plotted (or the data thats behind the polygons)
  output$proportion <- renderUI({
    
    selectInput("radio",
                 
                 label = h4("Choose View"),
    
                 choices = list("Cases per 100000" = 1,
                                
                                "Total Cases" = 2,
                                
                                "Age Range" = 3,
                                
                                "Cases to Age Range (per 100000)" = 4,
                                
                                "Predictive Analysis (colours emphasised to show hotspots)" = 5,
                                
                                "3 Day % Increase" = 6), 
                 selected = 1)
    
  })
  
  # Returns the latest column name in set.t / data.in which is the last dtg that data was pulled into the ap
  output$dtg.text <- renderText({
    
    req(!is.null(input$dtg.server))
    
    paste0("Last updated on ",(data.in()@data %>% colnames)[input$dtg.server %>% as.numeric()+os2])
  
    })
  
  # Not actually a UI function itself, it manages teh output from the action button panel
  bselect <- reactive({
    # on opening the program out doesn't actually exists, and thus needs a value
    if(exists("out") == FALSE) out <- 1
    if(input$act1 > max(act1)) out <- 1
    if(input$act2 > max(act2)) out <- 2
    if(input$act3 > max(act3)) out <- 3
    if(input$act4 > max(act4)) out <- 4
    if(input$act5 > max(act5)) out <- 5
    if(input$act6 > max(act6)) out <- 6
    
    # if action button is pressed, overwrite global variable 
    if(input$act1 > 0){
      act1 <- c(act1,input$act1)
      assign("act1", act1, envir = .GlobalEnv)
    }
    
    if(input$act2 > 0){
      act2 <- c(act2,input$act2)
      assign("act2", act2, envir = .GlobalEnv)
    }
    
    if(input$act3 > 0){
      act3 <- c(act3,input$act3)
      assign("act3", act3, envir = .GlobalEnv)
    }
    
    if(input$act4 > 0){
      act4 <- c(act4,input$act4)
      assign("act4", act4, envir = .GlobalEnv)
    }
    
    if(input$act5 > 0){
      act5 <- c(act5,input$act5)
      assign("act5", act5, envir = .GlobalEnv)
    }
    
    if(input$act6 > 0){
      act6 <- c(act6,input$act6)
      assign("act6", act6, envir = .GlobalEnv)
    }
    
    out
    
  })
  
  # pass bselect variable to UI for use in conditional formatting
  output$id<-reactive({bselect()})
  
  ####################################################################################################
  ####################################################################################################
  # BASE MAP AND POLYGON PLOTTING
  
  # create base map so that we dont need to keep reloading
  output$mymap <- renderLeaflet({
    
      leaflet() %>%
      
      setView(lng = 2, lat = 55, zoom = 5) %>%
      
      addTiles()
    
  })
  
  # plot polygon layers depending on input
  observe({
    
    req(!is.null(input$dtg.server))
    
    #req(!is.null(input$radio))
    
    # TESTING AREA
    # if action button is pressed it should be greater than it's historical outputs

    
    # used to define pal.r (it creates a vector of all the numbers that might be passed to pal.r as the domain for pal.r) 
    fil.d <- (100000 * data.in()@data[,(os2+1):ncol(data.in()@data)] / data.in()@data[,(os1-1)]) %>% round(.,digits = 0)
      # pallette for displaying age  
      pal.o <- colorBin("Blues", domain = log(age.df()[,4],base = exp(1)))
      # pallette for displaying the proportion of cases to popultion subset. 
      pal.p <- colorBin(colorRampPalette(brewer.pal(9, "PuRd"))(nrow(set.t@data)), domain = log(age.df()[,5], base = exp(1)))#colorBin("Reds", domain = log(age.df()[,5], base = exp(1)))
      # palette for displaying absolute cases
      pal.a <- colorBin("Reds", domain = data.in()@data[,(os2+1):ncol(data.in()@data)] %>% unlist)
      # palette for displaying cases per 100000
      pal.r <- colorBin("Reds", domain = fil.d %>% unlist )
      # paletter for displaying predictive increase
      pal.pred <- colorBin("Oranges", domain = pred.df()[,2])
      # paletter for displaying percentage increase. Log taken in order to dampen outliers
      pal.inc <- colorBin("Reds", domain = log(inc.df()[,2]))
      
      # add polygons to map
      leafletProxy("mymap", data = data.in()) %>%
      
      clearShapes() %>%
        
      addPolygons(weight = 0,
                  
                  # cases per 100000
                  fillColor = if(bselect() == 1){ 
                    
                  data.in()@data[,input$dtg.server+os2] %>% div.mul(.,data.in()@data[,(os1-1)]) %>% pal.r
                  
                  # cases total
                  } else if(bselect() == 2){
                    
                    data.in()@data[,input$dtg.server+os2] %>% pal.a
                  
                  # age distribution around country  
                  } else if(bselect() == 3){
                  
                     log(age.df()[,4],base = exp(1)) %>% pal.o

                  # cases to age distribution around country
                  } else if(bselect() == 4){
                    
                    log(age.df()[,5],base = exp(1)) %>% pal.p
                    
                  } else if (bselect() == 5){
                    
                    pred.df()[,2] %>% pal.pred
                    
                  }else if (bselect() == 6){
                    
                    inc.df()[,2] %>% log %>% pal.inc
                    
                  },  
                    
                  popup = paste(sep = "<br/>",
                                
                                paste0("<b>",data.in()@data[,2],"</b>"),
                                "",
                                
                                "COVID 19 Data", 
                                
                                paste0(data.in()@data[,input$dtg.server+os2]," total cases"),
                                
                                paste0(data.in()@data[,input$dtg.server+os2] %>% div.mul(.,data.in()@data[,(os1-1)])," cases per 100000"),
                                
                                "",
                                
                                "Demographic Information",
                                
                                paste0("Total Population: ",data.in()@data[,(os1-1)]),
                                
                                paste0("Percetange between ",input$slider.age[1]," and ", input$slider.age[2],": ",age.df()[,4],"%"),
                                
                                "",
                                
                                "BETA DEV ~ 5 day Predictive Analysis ~ BETA DEV",
                                
                                paste0("Based on projections of UK and European data. Still in development.",
                                       data.in()@data[,2],
                                       " will likely have between ",
                                       pred.df()[,2],
                                       " and ",
                                       pred.df()[,3],
                                       " cases per 100000 in ",
                                       input$pred,
                                       " days."
                                ),
                                "",
                                
                                paste0("Data for ",data.in()@data[,2]," last updated in P153 at ", data.in()@data[,3],". ",data.in()@data[,5]),
                                
                                paste(sep = "", "<b><a href='",data.in()@data[,4] , "'>", "link to source", "</a></b>")
                                
                                ),
                  
                  label = data.in()@data[,2],
                  
                  smoothFactor = 0.6,
                  
                  fillOpacity = 0.8
                  
                  ) 
    
  })
  
  ####################################################################################################
  ####################################################################################################  
  # COMPANIAN TABLE AND DATA TAB TABLE
  
  # create companion table
  output$table <- renderDataTable({
    
    #req(!is.null(input$radio))
    
    # prep data for table in order to show either proportional or total cases
    if(bselect() == 1){
      
      # returns names, total population and cases
      dt <- data.in()@data[,c(2,(os1-1),input$dtg.server+os2)]
      
      dt[,3] <- div.mul(dt[,3],dt[,2])
      
      # order data for table
      dt <- dt[order(-dt[,3]),]
      
      colnames(dt) <- c("Area","Total Population","Confirmed Cases per 10000")

      } else if(bselect() == 2){
        
      dt <- data.in()@data[,c(2,(os1-1),input$dtg.server+os2)]
    
      # order data for table
      dt <- dt[order(-dt[,3]),]
      
      colnames(dt) <- c("Area","Total Population","Total Confirmed Cases")
      
      } else if(bselect() == 3 | bselect() == 4){
        
        
        dt <- age.df()[,c(2:5)]
        
        colnames(dt) <- c("Area",
                          "Total Population",
                          paste0("% between ",input$slider.age[1]," and ",input$slider.age[2]),
                          paste0("cases ratio for ",input$slider.age[1]," - ",input$slider.age[2])
                          )
        
        dt <- dt[order(-dt[,3]),]
        
      } else if(bselect() == 5){
        
        dt <- pred.df()
        
        colnames(dt) <- c("Area",
                          paste0("Cases per 100000 est in ",input$pred," days (lower)"),
                          paste0("Cases per 100000 est in ",input$pred," days (higher)"),
                          paste0("Crit Care Beds Req Est in ",input$pred," days (higher)"),
                          paste0("Crit Care Beds Req Est in ",input$pred," days (higher)")
        )
      # percentage increase over 3 days
      }  else if(bselect() == 6){
        
        dt <- inc.df()
        
        colnames(dt) <- c("Area", "Percentage increase over last 3 days")
        
        dt <- dt[order(-dt[,2]),]
        
      }
    
    datatable(dt, rownames = FALSE)
  
    })
  
  # create data table for download
  output$table.out <- renderDataTable({
    
    datatable(
      
      {data.in()@data},
      
      rownames = FALSE,
      
      )
    
  })
  
  # download handler
  output$downloadData <- downloadHandler(
    
    filename = function() {
    
        paste0("Data_gathered_and_aggregated_by_Paddy_Mullany_as_at_",Sys.time() %>% gsub(":","-",.) %>% gsub(" ","_",.), ".csv")
    
      },
    
    content = function(file) {
    
        write.csv(data.in()@data, file, row.names = FALSE)
    
      }
  )

}

