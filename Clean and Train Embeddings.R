# Clean text for word embeddings 
# Train Word Embeddings

library(textclean)
library(quanteda)
library(dplyr)
library(stringi)
library(stringr)

message('loading file')
raw_text <- readr::read_delim("~/Documents/LSE/Capstone/Data/harvey_twitter_text.txt",
                              delim= " ",
                              col_names = FALSE)

message('converting to vector')
raw_text <- raw_text$X1 %>% as.vector()

message('starting TEXCLEAN cleaning process...')
for (i in 1:length(raw_text)){
  clean_string <- raw_text[i]
  clean_string <- str_remove(clean_string,'RT')
  clean_string <- tolower(clean_string)
  clean_string <- replace_tag(clean_string)
  clean_string <- replace_url(clean_string)
  clean_string <- replace_emoji(clean_string)
  clean_string <- trimws(clean_string,which='both')
  raw_text[i] <- clean_string
  if (i %% 1000 == 0){
    message(paste0(i,'/',length(raw_text), ' strings cleaned'))
  }
}

message('finished cleaning')


message('starting QUANTEDA cleaning process...')
my_corpus <- corpus(raw_text)
my_tokens <- tokens(my_corpus,remove_punct=TRUE)

training_text <- sapply(my_tokens, paste, collapse=" ") %>% as.vector()

write(x = training_text,file="~/Documents/LSE/Capstone/Data/training_text.txt")
message('text has been written, BOSS')

library(rword2vec)
library(lsa)

message("training word embeddings model :)")
model <- word2vec(
  train_file = "/Users/andresgf91/Documents/LSE/Capstone/Data/harvey_twitter_text.txt",
  output_file = "/Users/andresgf91/Documents/LSE/Capstone/Data/harvey_twiter_text.bin",
  binary=1,
  num_threads=3,
  debug_mode=1)

message("finished training word embeddings model")


#------------------------


library(rword2vec)
library(lsa)

 
notify_me <- function (Title = "Code Finished Running",
                       MSG = 'Check to see if all ok', 
                       Sound = 10) {
  require(notifier)
  require(beepr)
  
  beep(sound = Sound)
  notify(title = Title,
         msg = MSG) 
}

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/Embeddings")
message("training word embeddings model :)")
model <- word2vec(
  train_file = "training_text_9M_harvey_tweets.txt",
  output_file = "harvey_twitter_embeddings_10M_tweets.bin",
  binary=1,
  num_threads=4,
  debug_mode=1,layer1_size = 100,window = 5,cbow = 1)

message("finished training word embeddings model")
notify_me("finished training word embeddings model")
