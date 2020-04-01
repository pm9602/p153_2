
library(ggplot2)
library(dplyr)
library(scales)


###################################################################################################################################

# ICU BED VS CASES - ITALY

ICU_beds <- readxl::read_xls("italy_ICUbeds.xls")

# HYPOTHESES 1 - beds = ln(CASES * B) + C

# START IS A RESULT OF FITTING A  LINEAR MODEL TO EXP(CASES) = A * CASES + B

model.0 <- lm(exp(beds/1000) ~ cases,
              data = ICU_beds)

ICU_beds.i <- ICU_beds[1:(nrow(ICU_beds)-4),]

start <- list(a = coef(model.0)[1], b=coef(model.0)[2])

# HYPOTHESES 2 - beds = q * CASES ^ r
out <- nls(beds ~ (q * cases) ^ r,
           
           data = ICU_beds,
           
           start = list(q = 0.001, r = .5),
           
           control = list(maxiter = 5000)
           
)


# Hypothesis 3 - combination of linear model and q * cases ^ r

ggplot() + 
  
  geom_point(data=ICU_beds, aes(x=cases, y=beds, colour = "blue")) + 
  
  geom_point(data=ICU_beds, aes(x=cases, y= 1000 * log(start$b * cases + start$a), colour = "orange")) + 
  
  geom_point(data=ICU_beds, aes(x=cases, y= sapply(cases, ICU.func), colour = "green")) + 
  
  scale_color_identity(name = "Model fit",
                       
                       breaks = c("blue", "orange", "green"),
                       
                       labels = c("Beds vs Cases", "Beds = A * ln(B * Cases + C)", "Beds = A * Cases ^ B"),
                       
                       guide = "legend") + 
  
  scale_y_continuous(limits = c(0, NA), expand = c(0,0)) + 
  
  ggtitle("Cases vs Beds Assesments")


#####################################################################################################################
# CASE GROWTH ESTIMATES

cases_world <- readxl::read_xls("world_data.xls")

cases_world$Date <- cases_world$Date %>% as.Date

# SET 1
toMatch <- paste(sep = "|", "United Kingdom","Germany","France","Ireland","Spain","Italy")

cases_set1 <- cases_world$Entity %>% {grep(toMatch,.)} %>% cases_world[.,]

# population per country in 100000
pop_set1 <- list(UK = c(664.4,"United Kingdom"),
                 DE = c(827.9,"Germany"),
                 FR = c(669.9,"France"),
                 ES = c(446.6,"Spain"),
                 IT = c(604.8,"Italy"),
                 IE = c(48.3, "Ireland")
)

colnames(cases_set1)[4] <- "cases"

ggplot() +
  
  geom_point(data=cases_set1$Entity %in% (pop_set1["UK"] %>% unlist())[2] %>% cases_set1[.,],
             aes(x=Date,
                 y=cases/(pop_set1["UK"] %>% unlist())[1] %>% as.numeric,
                 colour = "blue")
  ) + 
  
  geom_point(data=cases_set1$Entity %in% (pop_set1["IT"] %>% unlist())[2] %>% cases_set1[.,],
             aes(x=Date,
                 y=cases/(pop_set1["IT"] %>% unlist())[1] %>% as.numeric,
                 colour = "green")
  ) + 
  
  geom_point(data=cases_set1$Entity %in% (pop_set1["IE"] %>% unlist())[2] %>% cases_set1[.,],
             aes(x=Date,
                 y=cases/(pop_set1["IE"] %>% unlist())[1] %>% as.numeric,
                 colour = "orange")
  ) + 
  
  geom_point(data=cases_set1$Entity %in% (pop_set1["ES"] %>% unlist())[2] %>% cases_set1[.,],
             aes(x=Date,
                 y=cases/(pop_set1["ES"] %>% unlist())[1] %>% as.numeric,
                 colour = "yellow")
  ) + 
  
  geom_point(data=cases_set1$Entity %in% (pop_set1["FR"] %>% unlist())[2] %>% cases_set1[.,],
             aes(x=Date,
                 y=cases/(pop_set1["FR"] %>% unlist())[1] %>% as.numeric,
                 colour = "red")
  ) + 
  
  geom_point(data=cases_set1$Entity %in% (pop_set1["DE"] %>% unlist())[2] %>% cases_set1[.,],
             aes(x=Date,
                 y=cases/(pop_set1["DE"] %>% unlist())[1] %>% as.numeric,
                 colour = "black")
  ) + 
  
  scale_color_identity(name = "Ctry",
                       breaks = c("blue","green","orange","yellow","red","black"),
                       labels = c("UK","IT","IE","ES","FR","DE"),
                       guide = "legend") +
  
  scale_x_date(date_breaks = "1 week", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2020-02-20',Sys.Date()))) + 
  
  labs(y= "date", x = "cases per 100000") + 
  
  ggtitle("Ctry cases history per 100000 people")


# normalise data i.e. italy had corona virus before anyone else, move everyone left to match italys start data

IT <- cases_set1$Entity %in% (pop_set1[c("IT")] %>% unlist())[2] %>% cases_set1$cases[.] %>% {which(. > 0)}

UK <- cases_set1$Entity %in% (pop_set1[c("UK")] %>% unlist())[2] %>% cases_set1$cases[.] %>% {which(. > 0)}

DE <- cases_set1$Entity %in% (pop_set1[c("DE")] %>% unlist())[2] %>% cases_set1$cases[.] %>% {which(. > 0)}

ES <- cases_set1$Entity %in% (pop_set1[c("ES")] %>% unlist())[2] %>% cases_set1$cases[.] %>% {which(. > 0)}

IE <- cases_set1$Entity %in% (pop_set1[c("IE")] %>% unlist())[2] %>% cases_set1$cases[.] %>% {which(. > 0)}

FR <- cases_set1$Entity %in% (pop_set1[c("FR")] %>% unlist())[2] %>% cases_set1$cases[.] %>% {which(. > 0)}


UK_set <- cases_set1$Entity %in% (pop_set1["UK"] %>% unlist())[2] %>% cases_set1[.,]
UK_set <- UK_set[UK,]
UK_set <- cbind(UK_set,1:length(UK))
colnames(UK_set)[5] <- "days"

IT_set <- cases_set1$Entity %in% (pop_set1["IT"] %>% unlist())[2] %>% cases_set1[.,]
IT_set <- IT_set[IT,]
IT_set <- cbind(IT_set,1:length(IT))
colnames(IT_set)[5] <- "days"

IE_set <- cases_set1$Entity %in% (pop_set1["IE"] %>% unlist())[2] %>% cases_set1[.,]
IE_set <- IE_set[IE,]
IE_set <- cbind(IE_set,1:length(IE))
colnames(IE_set)[5] <- "days"

ES_set <- cases_set1$Entity %in% (pop_set1["ES"] %>% unlist())[2] %>% cases_set1[.,]
ES_set <- ES_set[ES,]
ES_set <- cbind(ES_set,1:length(ES))
colnames(ES_set)[5] <- "days"

DE_set <- cases_set1$Entity %in% (pop_set1["DE"] %>% unlist())[2] %>% cases_set1[.,]
DE_set <- DE_set[DE,]
DE_set <- cbind(DE_set,1:length(DE))
colnames(DE_set)[5] <- "days"

FR_set <- cases_set1$Entity %in% (pop_set1["FR"] %>% unlist())[2] %>% cases_set1[.,]
FR_set <- FR_set[FR,]
FR_set <- cbind(FR_set,1:length(FR))
colnames(FR_set)[5] <- "days"


ggplot() +
  
  geom_point(data=UK_set, aes(x=days, y=cases, colour = "blue")) +
  
  geom_point(data=IT_set, aes(x=days, y=cases, colour = "green")) +
  
  geom_point(data=IE_set, aes(x=days, y=cases, colour = "orange")) +
  
  geom_point(data=ES_set, aes(x=days, y=cases, colour = "yellow")) +
  
  geom_point(data=FR_set, aes(x=days, y=cases, colour = "red")) +
  
  geom_point(data=DE_set, aes(x=days, y=cases, colour = "black")) +
  
  scale_color_identity(name = "Ctry",
                       
                       breaks = c("blue","green","orange","yellow","red","black"),
                       
                       labels = c("UK","IT","IE","ES","FR","DE"),
                       
                       guide = "legend") +
  
  labs(y= "cases per 100000", x = "Days from CORVID 19 first reported") + 
  
  ggtitle("Progress of CORVID 19 since first reported cases")


ggplot() +
  
  geom_point(data=UK_set[UK_set_start:length(UK),], aes(x=days, y=sqrt(cases), colour = "blue")) +
  
  geom_point(data=IT_set[IT_set_start:length(IT),], aes(x=days, y=sqrt(cases), colour = "green")) +
  
  geom_point(data=IE_set[IE_set_start:length(IE),], aes(x=days, y=sqrt(cases), colour = "orange")) +
  
  geom_point(data=ES_set[ES_set_start:length(ES),], aes(x=days, y=sqrt(cases), colour = "yellow")) +
  
  geom_point(data=FR_set[FR_set_start:length(FR),], aes(x=days, y=sqrt(cases), colour = "red")) +
  
  geom_point(data=DE_set[DE_set_start:length(DE),], aes(x=days, y=sqrt(cases), colour = "black")) +
  
  scale_color_identity(name = "Ctry",
                       
                       breaks = c("blue","green","orange","yellow","red","black"),
                       
                       labels = c("UK","IT","IE","ES","FR","DE"),
                       
                       guide = "legend") +
  
  labs(y= "sqrt(cases per 100000)", x = "Days from CORVID 19 first reported") + 
  
  ggtitle("Progress of CORVID 19 since first reported cases")

# take data from when cases are tested widly. This will be taken as when the data stands trending upwards on the log model

UK_set_start <- 28
IT_set_start <- 22
DE_set_start <- 29
ES_set_start <- 24
FR_set_start <- 32
IE_set_start <- 1

set.seed(20)

####################################################################################################################################
####################################################################################################################################

# UK MODELLING

# prep data for Hypothesis 1 and 2
UK_set <- cbind(UK_set,UK_set$cases/as.numeric(pop_set1$UK[1]))
UK_set <- cbind(UK_set,log(UK_set[,6]))
UK_set <- cbind(UK_set,log(UK_set$days))
colnames(UK_set)[6] <- "cases_prop"
colnames(UK_set)[7] <- "log_cases_prop"
colnames(UK_set)[8] <- "log_days"

UK_set.i <- UK_set[UK_set_start:length(UK),] 

#HYPOTHESIS 1 
# c is cases per 100000
# c = A * days ^ n
# ln(c) = ln(A) + n*ln(d)


UK_model <- lm(formula = log_cases_prop ~ log_days,
               data = UK_set.i)

# plot ln(c) = ln(A) + n*ln(d)
ggplot(data = UK_set.i) +
  
  geom_point(aes(x = log_days,y=log_cases_prop, color = "blue")) + 
  
  geom_point(aes(x = log_days,y= log_days * coef(UK_model)[2] + coef(UK_model)[1], color = "purple"), shape = 3) + 
  
  scale_color_identity(name = "Ctry",
                       
                       breaks = c("blue","purple"),
                       
                       labels = c("UK","estimate"),
                       
                       guide = "legend")

#HYPOTHESIS 2
# c = A * days ^ n + B


start_UK <- list(
  A_UK = exp(coef(UK_model)[1]),
  
  n_UK = coef(UK_model)[2],
  
  B = 0
)


UK_model2 <- nls(formula = cases_prop ~ A_UK * days ^ n_UK + B,
                 data = UK_set.i,
                 start = start_UK,
                 control = list(maxiter = 500))

summary(UK_model2)
UK_coefs <- coef(UK_model2)


# plot c = A * days ^ n + b
ggplot(data = UK_set.i) +
  
  geom_point(aes(x = days,y=cases_prop, color = "blue")) +
  
  geom_point(aes(x = days,y=sapply(days, uk.func1), color = "purple"), shape = 3) + 
  
  scale_color_identity(name = "Ctry",
                       
                       breaks = c("blue","purple"),
                       
                       labels = c("UK","estimate"),
                       
                       guide = "legend")

####################################################################################################################################
####################################################################################################################################

# ES MODELLING
ES_set <- cbind(ES_set,ES_set$cases/as.numeric(pop_set1$ES[1]))
ES_set <- cbind(ES_set,log(ES_set[,6]))
ES_set <- cbind(ES_set,log(ES_set$days))
colnames(ES_set)[6] <- "cases_prop"
colnames(ES_set)[7] <- "log_cases_prop"
colnames(ES_set)[8] <- "log_days"

ES_set.i <- ES_set[ES_set_start:length(ES),] 

#HYPOTHESIS 1 
# c is cases per 100000
# c = A * days ^ n
# ln(c) = ln(A) + n*ln(d)


ES_model <- lm(formula = log_cases_prop ~ log_days,
               data = ES_set.i)


# plot ln(c) = ln(A) + n*ln(d)
ggplot(data = ES_set.i) +
  
  geom_point(aes(x = log_days,y=log_cases_prop, color = "yellow")) + 
  
  geom_point(aes(x = log_days,y=corr_n * log_days * coef(ES_model)[2] + corr_A * coef(ES_model)[1], color = "purple"), shape = 3) + 
  
  scale_color_identity(name = "Ctry",
                       
                       breaks = c("yellow","purple"),
                       
                       labels = c("spain","estimate"),
                       
                       guide = "legend")

#HYPOTHESIS 2
# c = A * days ^ n + B

corr_n <- 1#.975

corr_A <- 1#.995

start_es <- list(
  A_es = exp(coef(ES_model)[1] * corr_A),
  
  n_es = coef(ES_model)[2] * corr_n,
  
  B = 0
)



ES_model2 <- nls(formula = cases_prop ~ A_es * days ^ n_es + B,
                 data = ES_set.i,
                 start = start_es,
                 control = list(maxiter = 500))

ES_coefs <- coef(ES_model2)

# plot c = A * days ^ n + B
ggplot(data = ES_set.i) +
  
  geom_point(aes(x = days,y=cases_prop, color = "yellow")) +
  
  geom_point(aes(x = days,y=sapply(ES_set.i$days, es.func1), color = "purple"), shape = 3) + 
  
  scale_color_identity(name = "Ctry",
                       
                       breaks = c("yellow","purple"),
                       
                       labels = c("spain","estimate"),
                       
                       guide = "legend")

####################################################################################################################################
####################################################################################################################################
# FUNCTIONS 

# ICU Beds per cases

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

# EXAMPLES

# lets say we have 50 cases per 100000, I want to know how many cases we will have in 5 days time
es.func2(50) %>% {es.func1(. + 5)} %>% round(.,digits = 0)
uk.func2(50) %>% {uk.func1(. + 5)} %>% round(.,digits = 0)

# now I want to know how many ICU beds I will need in 5 days time
es.func2(50) %>% {es.func1(. + 5)} %>% ICU.func(.)
uk.func2(50) %>% {uk.func1(. + 5)} %>% ICU.func(.)

# and now I want to predict how many ICU beds I will need


