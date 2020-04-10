#if(!require(devtools)) install.packages("devtools")

library(ggplot2)
library(dplyr)
library(readxl)
library(ggpubr)

# load NHS regional data and reorganise
########################################################################################################################
NHS_Regions <- readxl::read_xls("data/NHS_regions-deaths.xls") %>% as.data.frame()

# create seperate data frames for each region

# list of all NHS areas

NHS <- list(
  # East of England
  EoE = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`East of England - Confirmed Cases`,
                    
                    deaths = NHS_Regions$`East Of England - Deaths`)
  
  # London
  ,Lon = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`London - Confirmed Cases`,
                    
                    deaths = NHS_Regions$`London - Deaths`)
  # South East
  ,SoE = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`South East - Confirmed Cases`,
                    
                    deaths = NHS_Regions$`South East - Deaths`)
  # South West
  ,SoW = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`South West - Confirmed Cases`,
                    
                    deaths = NHS_Regions$`South West - Deaths`)
  
  # Midlands
  ,Mid = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`Midlands - Confirmed Cases` ,
                    
                    deaths = NHS_Regions$`Midlands - Deaths`)
  
  # North East and Yorkshire
  ,NE = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`North East and Yorkshire - Confirmed Cases`,
                    
                    deaths = NHS_Regions$`North East And Yorkshire - Deaths`)
  
  # North West
  ,SoE = data.frame(date = NHS_Regions$Date,
                    
                    confirmed_cases = NHS_Regions$`North West - Confirmed Cases`,
                    
                    deaths = NHS_Regions$`North West - Deaths`)

)

# create empty list for plots
I_pred_graphs <- vector(mode = "list", length = length(NHS))
names(I_pred_graphs) <- names(NHS)

# set parameters for back estimate for I_total
########################################################################################################################
' m = morality rate
q = time from showing symptons to death
d = time taken between showing symptons and being admitted to hospital
qn = normal duration of virus'


parameters_2 <- data.frame( m = c(0.05,0.01,0.02),
                            q = c(15,20,20),
                            d = c(6,3,6),
                            qn = c(15,10,15))

rownames(parameters_2) <- c("min","max","est")

# calulcate I_total for each list element
########################################################################################################################
# create empty columns for estimate of total cases (I_total)

for (ii in 1:length(NHS)) {

  NHS[[ii]]$I_total_min <- rep(0,nrow(NHS[[1]]))
  
  NHS[[ii]]$I_total_max <- rep(0,nrow(NHS[[1]]))
  
  NHS[[ii]]$I_total_est <- rep(0,nrow(NHS[[1]]))
  
    pp <- 1
    
    for(pp in 1:3){
        
      '1) given deaths (D) today, mortality rate (m), and when the patient first got sick (q), we can estimate that q days ago 
      there should have been D/m cases in the population.'
      #----------------------------------------------------------------------------------------------------------------------------------------
      # range 1.1 denotes where the values we are calculating go in teh data frame
      # range 1.2 denotes the values we are using to calculate total infections
      
      range1.1 <- (1:(nrow(NHS[[ii]]) - parameters_2$q[pp]))
      
      range1.2 <- range1.1 + parameters_2$q[pp]
      
      NHS[[ii]][range1.1,pp + 3] <- (range1.2 %>% NHS[[ii]][.,3]) / parameters_2$m[pp]
      
      '2) given number of cumulative confirmed cases today (I2 + I3), and the time between symptons showing and hospital 
      admission (d) (assuming the vast majority of testing has taken place), the rate of growth in tested cases should be similar to the rate of 
      growth in actual cases d days ago.'
      #----------------------------------------------------------------------------------------------------------------------------------------
      # range 2.1 denotes where the values we are calculating go in teh data frame
      # range 2.2 & 2.3 denotes the values we are using to calculate total infections
      
      range2.1 <- (nrow(NHS[[ii]]) - parameters_2$q[pp] + 1):(nrow(NHS[[ii]]) - parameters_2$d[pp])
      
      range2.2 <- range2.1 + parameters_2$d[pp]
      
      range2.3 <- range2.2 - 1
      
      # increase of cases d days ago, day by day
      perc_inc <- ((NHS[[ii]][range2.2,2] - NHS[[ii]][range2.3,2])/NHS[[ii]][range2.3,2] + 1)
      
      # total accumulated increase
      tot_inc <- rep(0,length(perc_inc))
      for (i in 1:length(perc_inc)) tot_inc[i] <- prod(perc_inc[1:i])
      
      NHS[[ii]][range2.1,3+pp] <- NHS[[ii]][max(range1.1),3+pp] * tot_inc
      
      ' 3) This still leaves a few days, which are calculated using an average rate from the last 3 days (note this is taken from caulcated 
      cases, so is an average of an estimate taken on wobbly assumptions'
      #----------------------------------------------------------------------------------------------------------------------------------------
      # range 3.1 denotes where the values we are calculating go in teh data frame
      
      range3.1 <- (max(range2.1) + 1) : nrow(NHS[[ii]])
      
      perc_inc_last <- mean(perc_inc[(length(perc_inc)-3):length(perc_inc)])
      
      perc_inc_all <- perc_inc_last ^ (1:(length(range3.1))) 
      
      NHS[[ii]][range3.1,3+pp] <- perc_inc_all * NHS[[ii]][max(range2.1),3+pp]
      
      #----------------------------------------------------------------------------------------------------------------------------------------
      ' 4) We now need to take into account people getting better (i.e. people dropping out of population after qn days)'
    
      range4.1 <- (parameters_2$qn[pp]+1):nrow(NHS[[ii]])
      range4.2 <- 1:(nrow(NHS[[ii]]) - parameters_2$qn[pp])
      
      NHS[[ii]][range4.1,3+pp] <- NHS[[ii]][range4.1,3+pp] - NHS[[ii]][range4.2,3+pp] 
      
    }
    
    # CREATE GRAPHS
    ###########################################################################################################################################
    I_pred_graphs [[ii]] <- ggplot(data = NHS[[ii]])+
      
      geom_line(aes(x = date,y = I_total_min), color = "red") + 
      
      geom_line(aes(x = date,y = I_total_max), color = "blue") + 
      
      geom_line(aes(x = date,y = I_total_est), color = "green") + 
      
      labs(title = names(NHS)[ii]) + 
      
      xlab("date") + 
      
      ylab("I_total") + 
      
      theme_light()
}

ggarrange(I_pred_graphs[[1]],
          I_pred_graphs[[2]],
          I_pred_graphs[[3]],
          I_pred_graphs[[4]],
          I_pred_graphs[[5]],
          I_pred_graphs[[6]],
          I_pred_graphs[[7]],
          ncol = 2,
          nrow = 4)