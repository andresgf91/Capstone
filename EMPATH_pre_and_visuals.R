
#CREATE TEXT FILE THAT CAN BE READ INTO PYTHON FOR EMPATH BUSINESS----- ALSO CONTAINS RESULTS VISUALIZATION CODE 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone")
library(DBI)
SQL_path <- "Data/SQL_TEXAS_HARVEY.sqlite"
db <- dbConnect(RSQLite::SQLite(), SQL_path)

dbListTables(db)
dbListFields(db,'lwic')


text_df <- dbGetQuery(db,"SELECT clean_text as text, id_str, created_at, user_county FROM new_target_tweets")


text_pandas <- function(df){
  source("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/R_code/Real_Deal_Functions.R")
  require(lubridate)
  #convert date column to time format
  message("Converting date field to appropiate format")
  df$created_at <- as_date(df$created_at)
  message("Filtering dates")
  df <- df %>%  
    filter(created_at> as_date('2016-08-23'),
           created_at< as_date('2018-08-26'))
  
  #assign week
  message("Creating week(date) column")
  df$week <- df$created_at %>% floor_date(unit='week',week_start = 1)
  #identify disaster affected counties as per FEMA (this adds a column called `affected` to df)
  df <- apply_disaster_no_date(df)
  return(df)}


text_df <- text_pandas(text_df)

text_df  <- text_df  %>%
  select(text,affected,week) %>%
  filter(affected=='affected')

library("data.table")
dt <- as.data.table(text_df)
dt <- dt[, list(text = paste(text, collapse="")), by = week]

dt <- as.data.frame(dt)

names(dt)

write.csv(dt,"~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/LDA/text_weekly_4_empath.csv")

library(rjson)
x <- toJSON(unname(split(dt, 1:1)))
cat(x)

library(jsonlite)
jsonlite::write_json(dt,path = "~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/LDA/text_weekly_4_empath.json")


library(quanteda)
text <- test_df %>% corpus() %>% tokens(include_docvars = TRUE)

text <- tokens_group(text,group='created_at')
save(text,file='text_for_empat.rda')

#--------VISUALIZE RESULTS FROM EMPATH TOOLS (analyzed using python EMPATH interface)-----------

#-------COSTUM DF-----------
library(dplyr)
library(lubridate)
df<- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/LDA/text_weekly_empath_results_costum.csv",stringsAsFactors = F)

df <- t(df)
df <- df %>% as.data.frame(stringsAsFactors = F)
cat_names <- df[1,] %>% as.vector()
colnames(df) <- cat_names
df <- df[2:nrow(df),]
df$date <- df %>% rownames()
df$date <- df$date %>% stringi::stri_sub(2)
df$date <- df$date %>% as_date()

costum_df <- df

#______-MAIN DF_______
df<- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/LDA/text_weekly_empath_results.csv",stringsAsFactors = F)

df <- t(df)
library(dplyr)
df <- df %>% as.data.frame(stringsAsFactors = F)

cat_names <- df[1,] %>% as.vector()
colnames(df) <- cat_names
df <- df[2:nrow(df),]
df$date <- df %>% rownames()
df$date <- df$date %>% stringi::stri_sub(2)

library(lubridate)
df$date <- df$date %>% as_date()
df <- left_join(df,costum_df,by='date')

options(scipen=999)

plot_EMPATH <- function (metric){
  require(ggplot2)
  df$metric <- df[[metric]]
  df %>% ggplot(aes(x=date,y=scales::rescale(as.numeric(metric),to=c(1,100)))) +
    geom_line()+
    stat_smooth(span=0.5) +
    theme_classic() +
    geom_vline(xintercept = as_date("2017-08-24"),col='red',linetype=2) +
    labs(title=paste("EMPATH -",toupper(metric)),x="Week",y="Value")
}

save_pics<- function(gg,name){
  require(png)
  path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Figures/"
  pic_name<- paste0(path,name,".png")
  
  png(pic_name,units="in", width=8, height=5,res=1200, pointsize = 14)
  print(gg)
  dev.off()
}

df <- costum_df
names(df)

gg <-plot_EMPATH('crime')
gg <- plot_EMPATH('health')
gg <-plot_EMPATH('temporary_housing')

save_pics(gg,"EMPATH/temporary_housing")


gg <- plot_EMPATH('injury')
save_pics(gg,"EMPATH/injury")

gg <- plot_EMPATH('house_repairs')
save_pics(gg,"EMPATH/house_repairs")
gg <- plot_EMPATH('psych_counseling')
save_pics(gg,"EMPATH/psych_counseling")

#EXERCISE 
gg <- plot_EMPATH('exercise')
save_pics(gg,"EMPATH/exercise")

gg <- plot_EMPATH('insurance')
save_pics(gg,"EMPATH/insurance")


# folder_path <- "Data/Embeddings/Sub_dataframes/"
# files <- list.files(folder_path)
# ls <- list()
# for (i in files){
#   file_path <- paste0(folder_path,i)
#   load(file_path)
#   ls[[i]] <- embed_df
#   rm(embed_df)
# }
# 
# embed_df <- do.call(rbind,ls)
# embed_df <- distinct(embed_df)
# 
# dbWriteTable(db,'embeddings_df',embed_df)
