# 1. Clean text and add new column with clean_text to SQL DATABASE - 
# 2. ALSO TRAIN A WORD EMBEDDINGS MODEL with new clean_text

library(textclean)
library(quanteda)
library(dplyr)
library(stringi)
library(stringr)
library(DBI)

message('Loading text from SQL database...')
SQL_path <- "~/Capstone/Data/SQL_TEXAS_HARVEY.sqlite"
db <- dbConnect(RSQLite::SQLite(), SQL_path)
df <- dbGetQuery(db,"SELECT id_str, text FROM target_tweets")
dbDisconnect(db)
message("Closing connection to SQL database, for now...")


message('Converting to text to vector....')
raw_text <- df$text %>% as.vector()

message('Starting TEXCLEAN cleaning process...')
for (i in 1:length(raw_text)){
  clean_string <- raw_text[i]
  clean_string <- str_remove(clean_string,'RT')
  clean_string <- tolower(clean_string)
  clean_string <- replace_tag(clean_string)
  clean_string <- replace_url(clean_string)
  clean_string <- replace_emoji(clean_string)
  clean_string <- trimws(clean_string,which='both')
  raw_text[i] <- clean_string
  if (i %% 10000 == 0){
    message(paste0(i,'/',length(raw_text), ' Strings cleaned'))
  }
}

message('Finished text-cleaning, BOSS....')


message('Starting QUANTEDA cleaning process...')
my_corpus <- corpus(raw_text)
my_tokens <- tokens(my_corpus,remove_punct=TRUE)

raw_text <- sapply(my_tokens, paste, collapse=" ") %>% as.vector()

write(x = raw_text,file="~/Capstone/Data/clean_target_text.txt")
message('Text has been written, BOSS')

df$text <- raw_text
rm(raw_text)


db <- dbConnect(RSQLite::SQLite(), SQL_path)
dbWriteTable(db,'clean_target_text',df,overwrite=TRUE)
DBI::dbDisconnect(db)

rm(df)
message('Clean Target Text table has been added to SQL database, BOSS. \n
        Will proceed to train the word embeddings model now....')


library(rword2vec)
library(lsa)

message("Training word embeddings model :)")
model <- word2vec(
  train_file = "/Users/andresgf91/Capstone/Data/clean_target_text.txt",
  output_file = "/Users/andresgf91/Capstone/Data/clean_target_text.bin",
  binary = 1,
  num_threads = 3,
  debug_mode = 1)

message("Finished training word embeddings model")
