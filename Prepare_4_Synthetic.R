# CONTAINS FUNCTIONES NEEDED TO CREATE DATAFRAME THAT CAN BE RUN by GSYNTH PACKAGE 
# IT ALSO LOADS COVARIATE DATA AND ASSIGNS TO RESPECTIVE CONTIES


library(lubridate)
library(dplyr)
library(DBI)
library(stringr)
library(stringi)
library(gsynth)

# Load Datasets -----------------------------------------------------------
# GAS PRICES
#Texas All Grades All Formulations Retail Gasoline Prices Weekly
#Source: U.S. Energy Information Administration
#https://www.eia.gov/opendata/qb.php?category=240908&sdid=PET.EMM_EPM0_PTE_STX_DPG.W
gas_prices <- read.csv("~/Downloads/Texas_All_Grades_All_Formulations_Retail_Gasoline_Prices_Weekly.csv",
                       skip = 4,
                       stringsAsFactors = FALSE)

colnames(gas_prices) <- c('week','price')
gas_prices$week <- as.Date(gas_prices$week, format = "%m/%d/%y")


#UNEMPLOYMENT rate
#----------county level unemployment ------------- 
unemployment <- read.csv("~/Downloads/county_unemployment.csv",
                         stringsAsFactors = FALSE)
unemployment_2 <- read.csv("~/Downloads/county_unemployment_2.csv",
                         stringsAsFactors = FALSE)

unemployment <- rbind(unemployment,unemployment_2)
unemployment <- unemployment %>% distinct()
rm(unemployment_2)

unemployment$Period <- unemployment$Period  %>%  stri_sub(2)
unemployment$date <- zoo::as.yearmon(paste(unemployment$Year,
                                           unemployment$Period),"%Y %m")%>%
  as_date()

unemployment$county_id <- unemployment$Series.ID %>% stri_sub(4,-3)

id_name <- read.csv("~/Downloads/laucnty18.csv", stringsAsFactors = FALSE, skip = 4)

id_name <- id_name %>%
  filter(Code.1==48) %>% 
  select(Code,County.Name.State.Abbreviation) %>%
  rename('county_name'=County.Name.State.Abbreviation)

id_name$county_name <- id_name$county_name %>%
  stri_sub(1,-12) %>%
  tolower()



  
unemployment <- left_join(unemployment,
                  id_name,
                  by=c("county_id"="Code"))

unemployment <- unemployment %>%
  select(date,Value,county_name) %>%
  rename('unemployment_rate'=Value)

rm(id_name)

#unemployment %>% filter(date==as_date("2017-09-01"))

#POVERTY
poverty <- read.csv("~/Downloads/ACS_17_5YR_S1701/ACS_17_5YR_S1701_with_ann.csv",
                    stringsAsFactors = FALSE)
poverty <- poverty[-1,]
poverty <- poverty[,1:10]

#select county name and Percent below poverty level (ESTIMATE)
poverty <- poverty %>% select(GEO.display.label,HC03_EST_VC01) %>%
  rename('county_name' = GEO.display.label,
         'poverty_est' = HC03_EST_VC01)

poverty$county_name <- str_remove(poverty$county_name,' County, Texas') %>%tolower()
poverty$poverty_est <- as.numeric(poverty$poverty_est)

#POPULATION SIZE
county_pop <- read.csv("~/Downloads/texas_county_population.csv",stringsAsFactors = F)

county_pop$CTYNAME <- str_remove(county_pop$CTYNAME,' County') %>% tolower()
county_pop$CTYNAME[county_pop$CTYNAME=='dewitt'] <- 'dewitt'


#precipition 

# precipitation <- readr::read_delim("~/Downloads/asos.txt",delim=",")
# precipitation <- precipitation %>% rename('time_stamp'=valid)
# precipitation <- precipitation %>%
#   mutate(time_stamp=as_date(time_stamp))
# 
# precipitation <- precipitation %>%  
#   filter(time_stamp> as_date('2016-08-23'),
#          time_stamp< as_date('2018-08-26'))

library(maps)
library(stringi)
# message('getting county names for each station based on coordinates \n will take a bit SIR...')
# counties <-  map.where("county", precipitation$lon, precipitation$lat)
# 
# counties <- sub('.*,\\s*', '', counties) %>%
#  stri_replace_all_fixed(':main','')
# 
# dewits <- which(counties=='de witt')
# 
# if (length(dewits) > 0){
#   message("Some de witts identified in precipitation data \n Standarizing format...")
#   counties[dewits] <- 'dewitt'
# }
# 
# precipitation$county <- counties
# 
# precipitation <- precipitation %>%
#   filter(!is.na(county),p01m!='M',p01m!='T') %>%
#   mutate(p01m=as.numeric(p01m)) %>% 
#   select(county,time_stamp,p01m) %>%
#   group_by(county,time_stamp) %>% 
#   summarise(daily_precipitation=mean(p01m,na.rm = TRUE))
#   
# save(precipitation,file="~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/precipitation_counties.rda")
message('loading precipitations df')
load("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/precipitation_counties.rda")


# Create sub_functions ----------------------------------------------------

get_gas_prices <- function(df){
  df <- left_join(df,
                  gas_prices[,c('week','price')],
                  by=c("week"="week"))
  
  return(df)
}

get_unemployment <- function(df){
  
  
  df <- left_join(df,
                  unemployment,
                  by=c("month"="date","user_county"="county_name"))
  return(df)
}


get_population_size <- function (df){
  
  df <- left_join(df,
                  county_pop[,c('CTYNAME','Pop')],
                  by=c("user_county"="CTYNAME"))
  
  return(df)
}


missing_county_search <- function (df) {
  county_less <- which(is.na(df$user_county))
  affected_counties <- tolower(c("Aransas", "Austin", "Bastrop", "Bee", "Brazoria",
                                 "Caldwell", "Calhoun", "Chambers", "Colorado", "DeWitt",
                                 "Fayette", "Fort Bend", "Galveston", "Goliad", "Gonzales",
                                 "Grimes", "Hardin", "Harris", "Jackson", "Jasper", "Jefferson",
                                 "Karnes", "Kleberg", "Lavaca", "Lee", "Liberty", "Matagorda",
                                 "Montgomery", "Newton", "Nueces", "Orange", "Polk", "Refugio",
                                 "Sabine", "San Jacinto", "San Patricio", "Tyler", "Victoria",
                                 "Walker", "Waller", "Wharton"))
  set.seed(123)
  
  return(county_less)
}

create_weigths_df_pop <- function (df){
  county_pop <- county_pop %>% dplyr::filter(CTYNAME %in% df$user_county)
  total_pop <- sum(county_pop$Pop)
  actual_perc <- county_pop %>% group_by(CTYNAME) %>%
    summarise(actual_perc=Pop/total_pop*100)
  total_obs<- nrow(df)
  sample_perc <- df %>% group_by(user_county) %>% summarise(sample_perc=n()/total_obs*100)
  weight_df <- sample_perc %>% mutate(weight = actual_perc$actual_perc/sample_perc)
  message('weights df created')
  return(weight_df)
}

create_weigths_df <- function(df){
  require(dplyr)
weight_df <- df %>%
  group_by(user_county) %>%
  summarise(weight=n()/nrow(df))

return(weight_df)
}


get_weight <- function (df,weight_df=weight_df){
  
  df <- left_join(df,
                  weight_df[,c('user_county','weight')],
                  by=c("user_county"="user_county"))
  message("weights added")
  return(df)
}

get_poverty <- function (df){
  df <- left_join(df,
                  poverty[,c('county_name','poverty_est')],
                  by=c("user_county"="county_name"))
  return(df)
}

notify_me <- function (Title = "Code Finished Running",
                       MSG = 'Check to see if all ok', 
                       Sound = 10) {
  require(notifier)
  require(beepr)
  
  beep(sound = Sound)
  notify(title = Title,
         msg = MSG) 
}

get_precipitation <- function (df){
 df <- left_join(df,
                 precipitation,
                 by=c("created_at"="time_stamp","user_county"="county"))
  return(df)
 }

# CREATE MOTHER FUNCTION --------------------------------------------------


synthetic_df <- function (df){
  source("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/R_code/Real_Deal_Functions.R")
  
  #convert date column to time format
  message("Converting date field to appropiate format")
  df$created_at <- as_date(df$created_at)
  df$month <- df$created_at %>% floor_date(unit='month')
  #filter to make sure dataset spans two years
  message("Filtering dates")
  df <- df %>%  
  filter(created_at> as_date('2016-08-23'),
         created_at< as_date('2018-08-26'))
  
  #assign week
  message("Creating week(date) column")
  df$week <- df$created_at %>% floor_date(unit='week',week_start = 1)
  
  dewits <- which(df$user_county=='de witt')
  
  if (length(dewits) > 0){
    message("Some county name format discrepancies identified, standarizing format")
    df$user_county[dewits] <- 'dewitt'
  }
  #identify disaster affected counties as per FEMA (this adds a column called `affected` to df)
  df <- apply_disaster_no_date(df)

  #assign treatment (pre is one week before landfall/post is after landfall)
  message("Administering treatment to affected counties")
  df$treat_pre <- ifelse(df$affected == "affected" &
                         df$created_at >= as_date("2017-08-18"), 1, 0)
  df$treat_post <- ifelse(df$affected == "affected" &
                          df$created_at >= as_date("2017-08-24"), 1, 0)
  #gas_prices
  message('Retrieving state level weekly gas prices')
  df <- get_gas_prices(df)
  

  #missing_county_search
  missing <- missing_county_search(df)
  
  dewits <- which(df$user_county=='de witt')
  
  if (length(dewits) > 0){
    message("Some de witts identified, standarizing format")
    df$user_county[dewits] <- 'dewitt'
  }
  #unemployment
  message('Assigning county level monthly unemployment rates')
  df <- get_unemployment(df)
  #county population 
  message("Finding counties's population size")
  df <- get_population_size(df)
  
  weight_df <- df %>%
    group_by(user_county) %>%
    summarise(weight=n()/nrow(df))

  message("Assigning county weights")
  df <- get_weight(df,weight_df)
  
  message("Getting county poverty rates")
  df <- get_poverty(df)
  
  message("Getting county-level precipitation")
  df <- get_precipitation(df)
  
  message("HUGE success boss. Your dataframe is ready")
  return(df)
  
}


# GROUPING FUNCTIONS ------------------------------------------------------

group_by_county_day <- function (df,measure){
  df$measure <- df[[measure]]
  df <- df %>% select(created_at,affected,user_county,
                      unemployment_rate,
                      price,
                      measure,
                      weight,
                      poverty_est,
                      treat_post,
                      daily_precipitation) %>%
    group_by(created_at,user_county,affected) %>% summarise(unemployment=mean(unemployment_rate,na.rm = TRUE),
                                          gas_price=mean(price,na.rm = TRUE),
                                          measure=mean(measure,na.rm = TRUE),
                                          poverty=mean(poverty_est,na.rm = TRUE),
                                          rain=mean(daily_precipitation,na.rm = TRUE))
  
  df <- as.data.frame(df)
  return(df)
}

group_by_county_week <- function (df,measure){
  df$measure <- df[[measure]]
  df <- df %>% select(week,affected,user_county,
                      unemployment_rate,
                      price,
                      measure,
                      weight,
                      poverty_est,
                      treat_post,
                      daily_precipitation) %>%
    group_by(week,user_county,affected) %>% summarise(unemployment=mean(unemployment_rate,na.rm = TRUE),
                                                      gas_price=mean(price,na.rm = TRUE),
                                                      measure=mean(measure,na.rm = TRUE),
                                                      poverty=mean(poverty_est,na.rm = TRUE),
                                                      rain=mean(daily_precipitation,na.rm = TRUE))
  df <- as.data.frame(df)
  return(df)
}



group_by_county_month <- function (df,measure){
  df$measure <- df[[measure]]
  df <- df %>% select(month,affected,user_county,
                      unemployment_rate,
                      price,
                      measure,
                      weight,
                      poverty_est,
                      treat_post,
                      daily_precipitation) %>%
    group_by(month,user_county,affected) %>% summarise(unemployment=mean(unemployment_rate,na.rm = TRUE),
                                                       gas_price=mean(price,na.rm = TRUE),
                                                       measure=mean(measure,na.rm = TRUE),
                                                       poverty=mean(poverty_est,na.rm = TRUE),
                                                       rain=mean(daily_precipitation,na.rm = TRUE))
  df <- as.data.frame(df)
  return(df)
}


group_by_county_biweekly <- function (df,measure){
  df$measure <- df[[measure]]
  df <- df %>% select(created_at,affected,user_county,
                      unemployment_rate,
                      price,
                      measure,
                      weight,
                      poverty_est,
                      treat_post,
                      daily_precipitation) %>%
    mutate(biweek=floor_date(as_date(created_at),unit = period(2,'week'),week_start = 1)) %>% 
    group_by(biweek,user_county,affected) %>% summarise(unemployment=mean(unemployment_rate,na.rm = TRUE),
                                                       gas_price=mean(price,na.rm = TRUE),
                                                       measure=mean(measure,na.rm = TRUE),
                                                       poverty=mean(poverty_est,na.rm = TRUE),
                                                       rain=mean(daily_precipitation,na.rm = TRUE))
  df <- as.data.frame(df)
  return(df)
}








prepare_2_run_week <- function (df,measure){
  require(dplyr)
  
  g_df <- group_by_county_week(df,measure)
  g_df$treat_post <- ifelse(g_df$affected == "affected" &
                              g_df$week >= (floor_date(as_date("2017-08-24"),
                                                       unit='week',
                                                       week_start = 1)),1, 0)
  
  g_df <- g_df %>% arrange(week)
  g_df$time <- g_df$week %>% as.factor() %>% as.numeric()
  g_df$id <- g_df$user_county %>% as.factor() %>% as.numeric()
  
  g_df$Y <- g_df$measure %>% round(2)
  g_df$D <- g_df$treat_post
  g_df$X1 <- g_df$unemployment
  g_df$X2 <- g_df$rain

  
  weigth_df <- create_weigths_df(df)
  g_df <- get_weight(g_df ,weight_df = weigth_df)
  
  return(g_df)
}


prepare_2_run_month <- function(df,measure){
  require(dplyr)
  
  g_df <- group_by_county_month(df,measure)
  g_df$treat_post <- ifelse(g_df$affected == "affected" &
                              g_df$month >= (floor_date(as_date("2017-08-24"),
                                                       unit='month')), 1, 0)
  
  g_df <- g_df %>% arrange(month)
  g_df$time <- g_df$month %>% as.factor() %>% as.numeric()
  g_df$id <- g_df$user_county %>% as.factor() %>% as.numeric()
  
  g_df$Y <- g_df$measure %>% round(2)
  g_df$D <- g_df$treat_post
  g_df$X1 <- g_df$unemployment
  g_df$X2 <- g_df$rain
  
  
  weigth_df <- create_weigths_df(df)
  g_df <- get_weight(g_df,weight_df = weigth_df)
  
  return(g_df)
}


prepare_2_run_biweekly <- function(df,measure){
  require(dplyr)
  
  g_df <- group_by_county_biweekly(df,measure)
  g_df$treat_post <- ifelse(g_df$affected == "affected" &
                              g_df$biweek >= (as_date("2017-08-15")),1,0)
  
  g_df <- g_df %>% arrange(biweek)
  g_df$time <- g_df$biweek%>% as.factor() %>% as.numeric()
  g_df$id <- g_df$user_county %>% as.factor() %>% as.numeric()
  
  g_df$Y <- g_df$measure %>% round(2)
  g_df$D <- g_df$treat_post
  g_df$X1 <- g_df$unemployment
  g_df$X2 <- g_df$rain
  
  #g_df$X2[which(is.na(g_df$X2))] <- 0
  #g_df$Y[which(is.na(g_df$Y))] <- 0
  
  
  weigth_df <- create_weigths_df(df)
  g_df <- get_weight(g_df,weight_df = weigth_df)
  
  return(g_df)
}


prepare_2_run_daily <- function(df,measure){
  require(dplyr)
  
  g_df <- group_by_county_day(df,measure)
  g_df$treat_post <- ifelse(g_df$affected == "affected" &
                              g_df$created_at >= (as_date("2017-08-24")),1,0)
  
  g_df <- g_df %>% arrange(created_at)
  g_df$time <- g_df$created_at %>% as.factor() %>% as.numeric()
  g_df$id <- g_df$user_county %>% as.factor() %>% as.numeric()
  
  g_df$Y <- g_df$measure %>% round(1)
  g_df$D <- g_df$treat_post
  g_df$X1 <- g_df$unemployment
  g_df$X2 <- g_df$rain

  
  weigth_df <- create_weigths_df(df)
  g_df <- get_weight(g_df,weight_df = weigth_df)
  
  return(g_df)
}
#------------------------------------------

group_by_county_week_G <- function (df,measure){
  
  df$measure <- df[[measure]]
  df <- df %>% select(week,group,user_county,
                      unemployment_rate,
                      price,
                      measure,
                      G_W,
                      poverty_est,
                      treat_post,
                      daily_precipitation) %>%
    group_by(week,group) %>% summarise(unemployment=mean(unemployment_rate,na.rm = TRUE),
                                       gas_price=mean(price,na.rm = TRUE),
                                       measure=mean(measure,na.rm = TRUE),
                                       poverty=mean(poverty_est,na.rm = TRUE),
                                       rain=mean(daily_precipitation,na.rm = TRUE),
                                       weight=mean(G_W,na.rm=TRUE))
  df <- as.data.frame(df)
  return(df)
}



prepare_2_run_week_G <- function (df,measure,controls=6,MT=TRUE){
  require(dplyr)
  
  message('Grouping by counties')
  df <- get_county_groups(df,n=controls,MT=MT)
  
  group_weights <- df %>% group_by(group) %>% 
    summarise(G_W=n()/nrow(df))
  
  df <- left_join(df,group_weights, by=c("group"="group"))
  
  g_df <- group_by_county_week_G(df,measure)
  g_df$treat_post <- ifelse(g_df$group >50  &
                              g_df$week >= (floor_date(as_date("2017-08-24"),
                                                       unit='week',
                                                       week_start = 1)),1, 0)
  
  
  g_df <- g_df %>% arrange(week)
  g_df$time <- g_df$week %>% as.factor() %>% as.numeric()
  g_df$id <- g_df$group %>% as.numeric()
  
  g_df$Y <- g_df$measure %>% scales::rescale(to=c(0,100))
  g_df$D <- g_df$treat_post
  g_df$X1 <- g_df$unemployment
  g_df$X2 <- g_df$rain
  
  return(g_df)
}

get_county_groups <- function (df,n=6,MT=TRUE){
  control_counties <- df %>%
    filter(affected=='not_affected') %>%
    select(user_county) %>%
    unique() 
  
  control_counties$group <- 0
  set.seed(123)
  for (i in 1:nrow(control_counties)){
    control_counties$group[i] <- sample(1:n,1)
  }
  
  treated_counties <- df %>%
    filter(affected=='affected') %>%
    select(user_county) %>%
    unique()
  
  treated_counties$group <- 99
  
  if(MT==TRUE){
  treated_counties$group[treated_counties$user_county=='harris'] <- 98}
  
  group_counties <- rbind(treated_counties,control_counties)
  
  df <- left_join(df,
                  group_counties,
                  by=c("user_county"="user_county"))
  
  return(df)
  
}

#-----------------------------
get_gsynth_estimate <- function(df, Cores=6, Infer="parametric",botas=2000,W=TRUE) {
  require(gsynth)
  
  if (W==TRUE){
  result <- gsynth(Y ~ D + X1+ X2,
                   data = df,
                   index = c("id","time"),
                   force = "two-way",
                   estimator = "mc",
                   EM = TRUE,
                   CV = TRUE,
                   r = c(0, 5),
                   se = TRUE,
                   inference = Infer,
                   nboots = botas,
                   parallel = T,
                   cores=Cores,
                   weight = 'weight',
                   normalize = T,
                   na.rm=TRUE)
  }
  else{
    result <- gsynth(Y ~ D + X1+ X2,
                     data = df,
                     index = c("id","time"),
                     force = "two-way",
                     estimator = "mc",
                     EM = TRUE,
                     CV = TRUE,
                     r = c(0, 5),
                     se = TRUE,
                     inference = Infer,
                     nboots = botas,
                     parallel = T,
                     cores=Cores,
                     na.rm=TRUE)
  }
    

  
  return(result)
}

get_multiple_synth_results <- function(df, measure,Cores=6,Infer="parametric"){
  
  df <- synthetic_df(df)
  
  #df_day <- prepare_2_run_daily(df,measure)
  df_week <- prepare_2_run_week(df,measure)
  df_biweek <- prepare_2_run_biweekly(df,measure)
  df_month <-prepare_2_run_month(df,measure)
  
  dataframes <- list('week'=df_week, 'biweek'= df_biweek, 'month'=df_month)
  results <- list()
  
  for (i in 1:length(dataframes)){
    results[[i]] <- get_gsynth_estimate(dataframes[[i]],Cores=Cores,Infer=Infer)
  }
  message("FINISHED GETTING ALL RESULTS")
  return(results)
}



message("DATASETS AND FUNCTIONS HAVE BEEN LOADED, SIR.")


