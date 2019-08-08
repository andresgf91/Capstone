library(ROAuth,quietly = TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(tweetscores,quietly = TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(dplyr,quietly = TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(data.table,quietly = TRUE,verbose = FALSE,warn.conflicts = FALSE)

message('Packages loaded...')


load("~/Data/my_oauth")


source('~/Documents/LSE/Capstone/R_code/Andres_TweetScores.R')
message('Functions loaded...')

#load tweet ids
#set path to saving files JSON and Rdata
file_path <- '/Volumes/Pantera_Data/TWITTER_DATA/irma_dataverse_tweets_B1_2.json'
path_temp_completions <- '~/Documents/LSE/Capstone/Data/irma_dataverse_temporary_tweets_B1.rda'

#irma_ids <- read.delim('~/Documents/LSE/Capstone/Data/irma_filter_tweet_ids.txt')
#tweet_ids <- irma_ids %>% unique %>% unlist
#save(tweet_ids,file='~/Documents/LSE/Capstone/Data/irma_tweetsIDs_dataverse.rda')

#irma ids is name of variable
load('~/Documents/LSE/Capstone/Data/irma_tweetsIDs_dataverse.rda')
load('~/Documents/LSE/Capstone/Data/irma_tweetsIDs_dataverse_ALL_COMPLETED.rda')


tweet_ids <- tweet_ids[!(tweet_ids %in% all_completed)]

#load temporary completed ids 
load(path_temp_completions)
#load previous completed ids 

load('~/Documents/LSE/Capstone/Data/irma_dataverse_tweets_completed_B1.rda')
completed_ids <- unique(c(ids_completed,completed_ids)) %>% as.numeric()

#update completed round tweets Rdata file
save(completed_ids,
     file='~/Documents/LSE/Capstone/Data/irma_dataverse_tweets_completed_B1.rda')

#load original round completed ids
#load('/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/harvey_dataverse_completed_tweets.Rdata')

tweet_ids_1 <- tweet_ids[1:(length(tweet_ids)/4)]
tweet_ids_sub <- tweet_ids_1[!(tweet_ids_1 %in% completed_ids)]

message('Subset of tweet IDs identified...')

message(paste(length(tweet_ids_1)-length(tweet_ids_sub)),
        ' Tweets have already been downloaded for this batch...') #**


#remove unneccesary variables to save space
rm(tweet_ids)
rm(completed_ids)
rm(tweet_ids_1)
rm(ids_completed)
rm(all_completed)


start <- Sys.time()

message(paste('Starting mining process at', start))

#Run function
success <- 0
while(success==0) {
  
  
  tryCatch(
    {
      getStatuses_andres(tweet_ids_sub,
                         filename = file_path ,
                         oauth = my_oauth,
                         sleep = 0.5,
                         path_completed = path_temp_completions)
      success <- 1
    },
    error = function(cond) {
      message('There was an error, relaunching...')
      Sys.sleep(60)
    },
    warning = function(cond) {
      message(cond)
    },
    finally={
      load(path_temp_completions)
      #load previous completed ids 
      load('~/Documents/LSE/Capstone/Data/irma_dataverse_tweets_completed_B1.rda')
      completed_ids <- unique(c(ids_completed,completed_ids)) %>% as.numeric()
      save(completed_ids,
           file='~/Documents/LSE/Capstone/Data/irma_dataverse_tweets_completed_B1.rda')
      tweet_ids_sub <- tweet_ids_sub[!(tweet_ids_sub %in% completed_ids)]
      
    })    
}

end <- Sys.time()

#print time
print(round((end-start),2))

