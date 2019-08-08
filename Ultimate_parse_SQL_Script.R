# PARSE TWEEETS AND IDENTIFY SUB_SAMPLES USING PARALLEL COMPUTING 


library(DBI)
library(stringi)
library(dplyr)

parallel_parse_tweets <- function(files,
                                         clust_num=4,
                                         type='PSOCK',
                                         total_sessions = 1,
                                         sesh_num=1){
  require(ROAuth)
  require(streamR)
  require(foreach)
  require(doParallel)
  require(dplyr)
  
  myCluster <- makeCluster(clust_num, # number of cores to use
                           type = type) # type of cluster
  
  #register parallel
  registerDoParallel(myCluster)

  if(total_sessions>1){
    find_range <- function(x,session_num,tot_sessions) { 
      
      upper_range <- round(((x/total_sessions)*session_num))
      lower_range <- round(1+(x/total_sessions*(session_num-1)))
      return(lower_range:upper_range)
    }
    
    X <- length(files)
    files <- files[find_range(x=X,
                              tot_sessions = total_sessions,
                              session_num = sesh_num)] 
  }
  
  temp_list <- list() 
  
  df <- foreach (i=1:length(files),.combine='rbind') %dopar% {
    require(dplyr)
    #add each file in folder as a df to `temp_list`
    df <- streamR::parseTweets(files[i])
    temp_list[[i]] <- df
    #bind all dfs in temp_list into a single df
    df <- do.call(rbind, temp_list)}
  df <- distinct(df)
  stopCluster(myCluster)
  load("~/Capstone/Data/completed_files_ultimate.rda")
  completed_files <- c(completed_files,files)
  save(completed_files,file="~/Capstone/Data/completed_files_ultimate.rda")
  return(df)
}


harvey_find_TEXAS_counties <- function (df) {
  require(maps)
  require(stringi)
  require(dplyr)
  tweets <- df
  #tweets <- df %>% select(user_id_str, lon, lat)
  tweets <- tweets[!is.na(tweets$lat),]
  
  ids_5plus_geo_locations <- tweets %>%
    select(user_id_str) %>% 
    group_by(user_id_str) %>%
    summarise(count=n()) %>%
    filter(count>5)
  
  ids_3plus_geo_locations <- ids_3plus_geo_locations$user_id_str %>% unique()
  
  tweets <- tweets %>% 
    filter(user_id_str %in% ids_3plus_geo_locations)
  
  states <- map.where("state", tweets$lon, tweets$lat)
  
  tweets$state <- states #assign state
  
  texas_ids <- c()
  for (i in ids_5plus_geo_locations){
    temp_df <- tweets %>%
      select(user_id_str,state) %>% 
      filter(user_id_str==i) %>% 
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
  
  non_affected_counties <- tweets %>% select(county,state) %>% filter(state=='texas') %>% 
    filter(!(county %in% affected_counties)) %>% select(county)
  non_affected_counties <- non_affected_counties$county %>% unique()
  
  affected_county_ids <- c()
  for (i in texas_ids){
    temp_df <- tweets %>%
      select(user_id_str,county) %>% 
      filter(user_id_str==i) %>% 
      group_by(county,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(county)) %>%
      arrange(-count)
    if (temp_df$county[1] %in% affected_counties){
      affected_county_ids <- c(affected_county_ids,temp_df$user_id_str[1])
    }
  }

  non_affected_county_ids <- c()
  for (i in texas_ids){
    temp_df <- tweets %>%
      select(user_id_str,county) %>% 
      filter(user_id_str==i) %>% 
      group_by(county,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(county)) %>%
      arrange(-count)
    if (temp_df$county[1] %in% non_affected_counties){
      non_affected_county_ids <- c(non_affected_county_ids,temp_df$user_id_str[1])
    }
  }
  
  relevant_texas_users <- unique(c(non_affected_county_ids,affected_county_ids))
  
  texas_user_tweets <- df %>%
    filter(user_id_str %in% relevant_texas_users)
  
  texas_user_tweets$state <- 'texas'
  
  outputs <- list(tweets = texas_user_tweets,
                  texas_users = relevant_texas_users,
                  affected_users = affected_county_ids,
                  non_affected_users = non_affected_county_ids)
  
  return(outputs)
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


estimate_user_language <- function(df) { 
  require(dplyr)
  require(stringi)
  
  tweets <- df
  #tweets <- df %>% filter(user_state==my_state) %>% select(user_id_str,lat,lon)
  
  df$user_lang <- NA
  ids <- unique(tweets$user_id_str)
  
  for (i in 1:length(ids)){
    
    temp_df <- tweets %>%
      dplyr::select(user_id_str,lang) %>% 
        dplyr::filter(user_id_str==ids[i]) %>% 
      group_by(lang,user_id_str) %>% 
      summarise(count=n()) %>%
      filter(!is.na(lang)) %>%
      arrange(-count)
    
    temp_locations <- which(df$user_id_str==ids[i])
    df$user_lang[temp_locations] <- temp_df$lang[1]
  }
  #df <- df %>% select(user_id_str,user_lang) %>% dplyr::distinct()
  return(df)
}

message('functions loaded...')


parent_folders <- c("/Volumes/Pantera_Data/Pablo_Data/all_users/",
                    "/Volumes/Pantera_Data/Harvey_pablo_affected/affected_user_timelines/",
                    "/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/affected_users/")

file_paths <- c()
for (i in parent_folders){
  file_paths <- c(file_paths,
                  paste0(i,list.files(i,pattern = '.json')))
}

load("~/Capstone/Data/completed_files_ultimate.rda")
file_paths <- file_paths[!(file_paths %in% completed_files)]
message(paste(length(file_paths),'user files to parse on this session'))


SQL_path <- '~/Capstone/Data/SQL_META_HARVEY.sqlite'


n <- round(length(file_paths)/100,0)
message(paste("Will parse in", n,'rounds, BOSS'))
#load("~/Capstone/Data/latest_completed_ultimate_parsing.rda")
latest_completed <- 1

for (i in seq(latest_completed,n)){
  
  message('starting parallel process')
  df <- parallel_parse_tweets(files = file_paths,
                                     total_sessions = n, sesh_num = i,clust_num = 4)
  
  
  message(paste('Finished parsing round #',i))
  df <- df %>% select(-c(favorite_count,favorited,source,
                         listed_count,url,expanded_url,place_id,
                         country,utc_offset,user_url))
  db <- dbConnect(RSQLite::SQLite(), SQL_path)
  
  
  dbWriteTable(db,'all_raw_tweets', df, append=TRUE, overwrite=FALSE)
  message('raw tweets added to SQL')
  dbDisconnect(db)
  
  message('looking for texas tweets')
  find_outputs <- harvey_find_TEXAS_counties(df)
  
  message('estimating user counties')
  df <- estimate_user_county(find_outputs[['tweets']])
  
  message('estimating user language')
  df <- estimate_user_language(df)
  
  texas_users <- find_outputs[['texas_users']] %>% as.data.frame.vector()
  affected_users <- find_outputs[['affected_users']] %>% as.data.frame.vector()
  non_affected_users <- find_outputs[['non_affected_users']] %>% as.data.frame.vector()
  
  
  db <- dbConnect(RSQLite::SQLite(), SQL_path)
  message('writing stuff to SQL database')
  dbWriteTable(db,'all_texas_tweets', df, append=TRUE, overwrite=FALSE)
  dbWriteTable(db,'affected_users', affected_users, append=TRUE, overwrite=FALSE)
  dbWriteTable(db,'non_affected_users', non_affected_users, append=TRUE, overwrite=FALSE)
  dbWriteTable(db,'texas_users', texas_users, append=TRUE, overwrite=FALSE)
  
  message('Success SQL writing...\n')
  #close SQL connection 
  dbDisconnect(db)
  
  latest_completed <- i
  save(latest_completed,file = "~/Capstone/Data/latest_completed_ultimate_parsing.rda")
  
  message(paste(round(((i/n)*100),2),'% completed'))

}


