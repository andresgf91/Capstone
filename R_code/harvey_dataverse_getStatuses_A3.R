library(ROAuth,quietly = TRUE)
library(tweetscores,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(data.table,quietly = TRUE)

message('Packages loaded...')



load("~/Data/my_oauth_3")


source('~/Documents/LSE/Capstone/R_code/Andres_TweetScores.R')
message('Functions loaded...')

#load tweet ids
load('~/Documents/LSE/Capstone/Data/harvey_tweetsIDs_dataverse.rda')
load('~/Documents/LSE/Capstone/Data/harvey_tweetsIDs_dataverse_completed2date.rda')

tweet_ids <- tweet_ids[!(tweet_ids %in% all_completed)]

#load temporary completed ids 
load('/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/harvey_dataverse_temporary_A3.rda')

#load original previous rounds completed ids
load('/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/harvey_dataverse_completed_A3.rda')
completed_ids <- unique(c(ids_completed,completed_ids))

#update completed round tweets Rdata file
save(completed_ids,
     file='/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/harvey_dataverse_completed_A3.rda')

#merge two datasets to create latest list of completed id's
already_completed <- as.numeric(completed_ids)

tweet_ids_3 <- tweet_ids[(length(tweet_ids)/2):((length(tweet_ids)/4)*3)] #**
tweet_ids_sub <- tweet_ids_3[!(tweet_ids_3 %in% already_completed)] #**

message('Subset of tweet IDs identified...')

message(paste(length(tweet_ids_3)-length(tweet_ids_sub)),
        ' Tweets have already been downloaded for this batch...') #**

#remove unneccesary variables to save space
rm(tweet_ids)
rm(already_completed)
rm(completed_ids)
rm(tweet_ids_3) #**
rm(ids_completed)
rm(all_completed)

#set path to saving files JSON and Rdata
file_path <- '/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/harvey_dataverse_tweets_A3-2.json'
path_temp_completions <- '/Volumes/Pantera_Data/TWITTER_DATA/Harvey_Dataverse/harvey_dataverse_temporary_A3.rda'

start <- Sys.time()

message(paste('Starting mining process at', start))
#Run function
getStatuses_andres(tweet_ids_sub,
            filename = file_path ,
            oauth = my_oauth,
            sleep = 0.15,
            path_completed = path_temp_completions)

end <- Sys.time()

#print time
print(round((end-start),2))
