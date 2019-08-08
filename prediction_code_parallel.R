notify_me <- function (Title = "Code Finished Running",
                       MSG = 'Check to see if all ok', 
                       Sound = 10) {
  require(notifier)
  require(beepr)
  
  beep(sound = Sound)
  notify(title = Title,
         msg = MSG) 
}


library(dplyr)
list_of_frames <- list()
for (i in 1:60){
  file_name <- paste0("/Users/andresgf91/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/Embeddings/Sub_dataframes/",
                      'embeddings_df_',i,'_R3.rda')
  
  load(file_name)
  
  list_of_frames[[i]] <- embed_df
  message(i)
}
message("FINISHED, try do.call Rbind now")

embed_df <- do.call('rbind',list_of_frames)
embed_df <- dplyr::distinct(embed_df)
rm(list_of_frames)
notify_me('embed_df CREATED/LOADED')

library(SuperLearner)


load("/Users/andresgf91/cv_slV10_AUC_R2.rda")

find_range <- function(x,session_num,tot_sessions) { 
  
  upper_range <- round(((x/tot_sessions)*session_num))
  lower_range <- round(1+(x/tot_sessions*(session_num-1)))
  return(lower_range:upper_range)
}



get_predictions_parallel <- function (my_list,clust=5) {
  require(parallel)
  require(doParallel)
  require(foreach)
  require(stats)
  myCluster <- makeCluster(clust, # number of cores to use
                           type = 'FORK') # type of cluster
  
  registerDoParallel(myCluster)
  temp_list <- c()
  load('~/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/Embeddings/empty_vector.rda')
  list <- foreach (i=1:length(my_list),.combine='rbind') %dopar% {
    sub_df <- my_list[[i]]
    vectors <- sub_df[,2:101]
    names <- sub_df[,1]
    result <- predict(cv_slV10_AUC$AllSL$`1`,newdata = vectors,
                      onlySL = TRUE)
    result <- result$pred %>% as.numeric() %>% as.vector()
    result <- data.frame(ids=names,prediction=result)
    temp_list[[i]]<- result
  }
  stopCluster(myCluster)
  return(list)
}

sessions <- 15000
start <- Sys.time()
list_dfs <- list()
for (j in 1:sessions){
  sub_range <- find_range(nrow(embed_df),j,sessions)
  sub_embed <- embed_df[sub_range,]
  if (j %%1000 ==0){
    message(j)}
  list_dfs[[j]] <- sub_embed
}

notify_me("starting prediction process")

system.time({
  prediction_df <- get_predictions_parallel(list_dfs[1:100])
})
nrow(prediction_df)

save(prediction_df, file='/Users/andresgf91/Library/Mobile Documents/com~apple~CloudDocs/Capstone/Data/Embeddings/prediction_df.rda')

notify_me("Prediction process completed")
