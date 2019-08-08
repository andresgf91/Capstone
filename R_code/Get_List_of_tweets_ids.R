#TWEET IDS 

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone")
library(DBI)
SQL_path <- "Data/SQL_TEXAS_HARVEY.sqlite"
db <- dbConnect(RSQLite::SQLite(), SQL_path)
texas_tweet_ids <- dbGetQuery(db,"SELECT id_str FROM new_target_tweets")

write.table(texas_tweet_ids, file="~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/texas_tweet_ids.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)

dbListFields(db,"texas_users")


SQL_path <- "/Volumes/Pantera_Data/SQL_META_HARVEY_ALL_RAW.sqlite"
db <- dbConnect(RSQLite::SQLite(), SQL_path)

dbListTables(db)
dbListFields(db,'all_raw_tweets')
raw_tweet_ids <- dbGetQuery(db,"SELECT id_str FROM all_raw_tweets")

write.table(raw_tweet_ids, file="~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/raw_tweet_ids.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)



# GET DATES FOR TEXAS TWEETS PRIOR TO FILTERING 
SQL_path <- "/Volumes/Pantera_Data/Capstone Backup/SQL_TEXAS_HARVEY22Jul.sqlite"
db <- dbConnect(RSQLite::SQLite(), SQL_path)

dbListTables(db)
dbListFields(db,'all_texas_dates')


dates_texas <- dbGetQuery(db,"SELECT created_at FROM all_texas_dates")

library(ggplot2)
library(dplyr)
library(lubridate)
dates_texas$created_at  <- dates_texas$created_at %>%  lubridate::as_date()

dates_texas %>% sample_frac(.5) %>% ggplot(aes(created_at)) +
  geom_density(fill='royalblue') +
  theme_classic()+
  geom_rect(aes(xmin=as_date("2016-08-25"),
                xmax=as_date("2018-08-25"),
                ymin=-Inf, ymax=+Inf),
            fill='goldenrod2', alpha=0.3)



