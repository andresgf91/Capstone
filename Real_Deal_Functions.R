# THESE ARE SOME OF MY MULTIPUPOSE TOOLS OR HELPER FUNCTIONS I IMPLEMENTED FROM TIME TO TIME



library(dplyr)


# FUNCTION TO IDENTIFIES AFFECTED COUNTY USERS BY GEO_LOC--- 
harvey_county_users_search <- function (df) {
  require(maps)
  require(stringi)
  require(dplyr)
  
  tweets <- df %>% select(user_id_str, lon, lat)
  tweets <- tweets[!is.na(tweets$lat),]
  
  ids_5plus_geo_locations <- tweets %>%
    group_by(user_id_str) %>%
    summarise(count=n()) %>%
    filter(count>5)
  
  ids_5plus_geo_locations <- ids_5plus_geo_locations$user_id_str %>% unique()
  
  tweets <- tweets %>% 
    filter(user_id_str %in% ids_5plus_geo_locations)
  
  states <- map.where("state", tweets$lon, tweets$lat)
  
  tweets$state <- states #assign state
  
  texas_ids <- c()
  for (i in ids_5plus_geo_locations){
    temp_df <- tweets %>% filter(user_id_str==i) %>% 
      group_by(state,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(state)) %>%
      arrange(-count)
    if (temp_df$state[1] %in% 'texas'){
      texas_ids <- c(texas_ids,temp_df$user_id_str[1])
    }
  }
  
  
  tweets <- tweets %>% filter(user_id_str %in% texas_ids)
  
  counties <- map.where("county", tweets$lon,tweets$lat)
  
  #remove state and :main from string
  counties <- sub('.*,\\s*', '', counties) %>%
    stri_replace_all_fixed(':main','')
  
  tweets$county <- counties #assing counties
  
  affected_counties <- tolower(c("Aransas", "Austin", "Bastrop", "Bee", "Brazoria",
                                 "Caldwell", "Calhoun", "Chambers", "Colorado", "DeWitt",
                                 "Fayette", "Fort Bend", "Galveston", "Goliad", "Gonzales",
                                 "Grimes", "Hardin", "Harris", "Jackson", "Jasper", "Jefferson",
                                 "Karnes", "Kleberg", "Lavaca", "Lee", "Liberty", "Matagorda",
                                 "Montgomery", "Newton", "Nueces", "Orange", "Polk", "Refugio",
                                 "Sabine", "San Jacinto", "San Patricio", "Tyler", "Victoria",
                                 "Walker", "Waller", "Wharton"))
  
  affected_county_ids <- c()
  for (i in texas_ids){
    temp_df <- tweets %>% filter(user_id_str==i) %>% 
      group_by(county,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(county)) %>%
      arrange(-count)
    if (temp_df$county[1] %in% affected_counties){
      affected_county_ids <- c(affected_county_ids,temp_df$user_id_str[1])
    }
  }
  
  harvey_affected_user_tweets <- df %>%
    filter(user_id_str %in% affected_county_ids)
  
  return(harvey_affected_user_tweets)
}

estimate_user_state <- function(df) { 
  require(dplyr)
  require(stringi)
  require(maps)
  
  tweets <- df
  tweets <- tweets[!is.na(tweets$lat),]
  
  states <-  map.where("state", tweets$lon, tweets$lat)
  tweets$state <- states
  
  df$user_state <- NA
  ids <- unique(tweets$user_id_str)
  
  for (i in 1:length(ids)){
    
    temp_df <- tweets %>% filter(user_id_str==ids[i]) %>% 
      group_by(state,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(state)) %>%
      arrange(-count)
    
    temp_locations <- which(df$user_id_str==ids[i])
    df$user_state[temp_locations] <- temp_df$state[1]
  }
  
  return(df)
  
}

estimate_user_county <- function(df) { 
  require(maps)
  require(dplyr)
  require(stringi)
  
  tweets <- df
  #tweets <- df %>% filter(user_state==my_state) %>% select(user_id_str,lat,lon)
  
  tweets <- tweets[!is.na(tweets$lat),]
  
  counties <-  map.where("county", tweets$lon, tweets$lat)
  counties <- sub('.*,\\s*', '', counties) %>%
    stri_replace_all_fixed(':main','')
  
  tweets$county <- counties
  
  df$user_county <- NA
  ids <- unique(tweets$user_id_str)
  
  for (i in 1:length(ids)){
    
    temp_df <- tweets %>%
      select(user_id_str,county) %>% 
      filter(user_id_str==ids[i]) %>% 
      group_by(county,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(county)) %>%
      arrange(-count)
    
    temp_locations <- which(df$user_id_str==ids[i])
    df$user_county[temp_locations] <- temp_df$county[1]
  }
  
  return(df)
}

users_county <- function(df){
  require(dplyr)
  
  df <- estimate_user_state(df)
  message('found states...\n')
  df <- estimate_user_county(df)
  message('found counties...\n')
  df <- df %>%
    group_by(user_id_str, user_state, user_county) %>%
    summarise()
  message('finished!')
  
  return(df)
}



estimate_user_language <- function(df) { 
  require(dplyr)
  require(stringi)
  
  tweets <- df
  #tweets <- df %>% filter(user_state==my_state) %>% select(user_id_str,lat,lon)
  
  df$user_lang <- NA
  ids <- unique(tweets$user_id_str)
  
  for (i in 1:length(ids)){
    
    temp_df <- tweets %>% filter(user_id_str==ids[i]) %>% 
      group_by(lang,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(lang)) %>%
      arrange(-count)
    
    temp_locations <- which(df$user_id_str==ids[i])
    df$user_lang[temp_locations] <- temp_df$lang[1]
  }
  df <- df %>% select(user_id_str,user_lang) %>% dplyr::distinct()
  return(df)
}



#---------CHANGE TWITTER DATE FIELD TO PROPER FORMAT-------------------------

change_date_format <- function (df){
  require(dplyr)
  require(lubridate)
  require(stringi)
  df$created_at <- stri_replace_first_regex (
    df$created_at,
    stri_extract_first_words(df$created_at),
    stri_extract_last_words(df$created_at))
  
  df$created_at <- stri_sub(df$created_at,1,20) %>%
    lubridate::ymd_hms()
  df$date <- df$created_at %>% as_date()
  return(df)
}



# Preprocess to save into SQLite database ---------------------------------



the_works <- function (df) {
  require(tweetscores)
  total_users <- length(unique(df$user_id_str))
  df <- harvey_county_users_search(df)
  affected_users <- length(unique(df$user_id_str))
  message(paste(total_users-affected_users,'users removed'))
  message(paste(affected_users,'affected users found'))
  message('changing date_format now')
  df$created_at <- formatTwDate(df$created_at)
  return(df)
}



cut_time <- function (df, after='2016-08-24', before='2018-08-26'){
  require(lubridate)
  total_tweets <- nrow(df)
  df <- df %>% filter(created_at > date(after),created_at < date(before))
  date_relevant_tweets <- nrow(df)
  message(paste(total_tweets-date_relevant_tweets,'Tweets removed'))
  message(paste(date_relevant_tweets,'Tweets between',after,'-',before))
  return(df)
  
}




merge_lat_lon <- function(df) {
  require(dplyr)
  
  #finds which rows dont have lat, lon but do have place_lat/lon and adds to lat/lon
  nas <- which(is.na(df$lat)) %>% as.data.frame.vector()
  have_values <- which(!is.na(df$place_lat))
  replacable <- nas %>% filter(. %in% have_values)
  replacable <- replacable$.
  
  df$lat[replacable] <- df$place_lat[replacable]
  df$lon[replacable] <- df$place_lon[replacable]
  return(df)
}


# visualize the disribution of time for tweets dataframe
plot_time_distribution <- function (df){
  require(ggplot2)
  gg <- df %>% ggplot(aes(created_at)) +
    geom_density() +
    geom_vline(xintercept = as.POSIXct("2017-08-25 12:00:00 UTC"), linetype = 4,col='blue') +
    theme_classic()
  
  return(gg)
}

#-------------------NOTIFY ME AND RING BELL ---------------
notify_me <- function (Title = "Code Finished Running",
                       MSG = 'Check to see if all ok', 
                       Sound = 10) {
  require(notifier)
  require(beepr)
  
  beep(sound = Sound)
  notify(title = Title,
         msg = MSG) 
}


#--------------FIND USERS THAT WERE AFFECTED AND ASSIGN AFFECT OR NOT 
apply_disaster_no_date <- function(df, add_weights='no'){
  require(dplyr)
  affected_counties <- tolower(c("Aransas", "Austin", "Bastrop", "Bee", "Brazoria",
                                 "Caldwell", "Calhoun", "Chambers", "Colorado", "DeWitt",
                                 "Fayette", "Fort Bend", "Galveston", "Goliad", "Gonzales",
                                 "Grimes", "Hardin", "Harris", "Jackson", "Jasper", "Jefferson",
                                 "Karnes", "Kleberg", "Lavaca", "Lee", "Liberty", "Matagorda",
                                 "Montgomery", "Newton", "Nueces", "Orange", "Polk", "Refugio",
                                 "Sabine", "San Jacinto", "San Patricio", "Tyler", "Victoria",
                                 "Walker", "Waller", "Wharton"))
  no_county <- which(is.na(df$user_county))
  affected_people <- which(df$user_county %in% affected_counties)
  non_affected_people <- which(!(df$user_county %in% affected_counties))
  df$affected <- NA
  df$affected[non_affected_people] <- 'not_affected'
  df$affected[affected_people] <- 'affected'
  df$affected[no_county] <- 'affected'
  
  message("DISASTER-affected counties identified and properly labeled per your request, BOSS")
  
  if (add_weights=='yes'){
    df$weight <- NA 
    df$weight[affected_people] <- 0.6393478
    df$weight[non_affected_people] <- 0.3606522
    df$weight[no_county] <- 0.6393478
  }
  
  return(df)
}

#---------------------------------------
plot_GI <- function (df, cat_name, date_var, UPPERCASE=TRUE){
  
  if(UPPERCASE==TRUE){
    cat_name <- toupper(cat_name)}
  
  df$date <- df[[date_var]]
  df$category <- df[[cat_name]]
  gg <- df %>% ggplot(aes(x = lubridate::as_date(date))) +
    geom_vline(xintercept = as_date("2017-08-25"),
               color='black', linetype = 4) +
    geom_smooth(data=df,aes(y=category, col=cat_name)) +
    theme_classic()
  
  return(gg)
}

#---------------------------------------


non_interest_locs <- c(', Mo',', MO', ", CA",", WI", ", CO", ", NYC", ", Ohio", "San Francisco",
                       ", KS", ", Oklahoma City", "Indianapolis", ", FL", ", Pakistan", "Mxico",
                       ", LA", ", MD", "New York", "New Jersey", ", HI", "Toronto",
                       ", WA", ", NE", ", VA", ", BC", ", GA", ", AZ", "lubbock, tx",
                       "Lubbock", "Wichita", "Fort Worth", "San Angelo, tx" ,
                       "Odessa, Texas", "Odessa, tx",
                       ", UT", ", OR", ", AL", ", NM", ", OK", ", LO", ", IO",", MI",
                       "Miami, FLorida",
                       "switzerland", "chicago", ", NJ", 'brooklyn','alaska',
                       'california','denver', 'dallas,Tx','dallas, tx' ,', TN',',D.F', 'Nigeria',
                       'Louisiana',', WA',', Jalisco', ', DC', 'ablerta', 'israel',
                       'Sao Paulo', 'latin america','Luanda-Angola','Denver, CO',
                       'Thailand','Hiroshima','new zealand',"mexico city","orlando via detroit ",
                       "guayas, gye ","indiana, usa" ,"maryland, earth",', ecuador',
                       "xsport fitness",', mi',"fulton county georgia","iowa, usa","lagos,nigeria",
                       ', kenya',', venezuela', ', south africa',
                       ', mexico', 'minnesota','argentina', 'canada','alabama',
                       "torrejón de ardoz","arizona, usa" ,', nv',', germany',
                       ', england', ', france', 'belize', 'costa rica',
                       'montreal', "colorado, usa",'jacksonville florida',', il',
                       'los angeles', ', malaysia', ', IN', ',ky', ', ky', ', japan',
                       ', NC', ', OH', ', NY', 'Boston', "Depok, Jawa Barat",
                       'Vancouver',"Mexico, D.F.",
                       ", Kuwait", ", NC", 'canberra', ', antioquia',', MS', ', IN',
                       'london', ', MA', 'montana','Philippines', ', ab','madrid',"Oregon/Nashville ",
                       'paraguay',', chile',"Missouri", 'moscow', 'norge', 'sydney',', jamaica',
                       ', SC',", Hawaii", 'Ohio') %>% tolower()