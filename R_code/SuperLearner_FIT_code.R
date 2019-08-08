#install.packages(c("caret", "glmnet", "randomForest", "ggplot2", "RhpcBLASctl"))
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(SuperLearner)
library(caret)
library(glmnet)
library(dplyr)
library(arm)

notify_me <- function (Title = "Code Finished Running",
                       MSG = 'Check to see if all ok', 
                       Sound = 10) {
  require(notifier)
  require(beepr)
  
  beep(sound = Sound)
  notify(title = Title,
         msg = MSG) 
}

#load('Training_df_4_ML.rda')
load("/Users/andresgf91/Downloads/rstudio-export11am/embeddings_df_4_ML2.rda")

#prepare embeddings
training_df <- sub_embeddings_df %>% filter(!is.na(my_code)) %>% filter(my_code <=1)
outcome <- as.numeric(training_df$my_code)


#prepare without embeddings
#training_df <- training_df %>% filter(!is.na(my_code)) %>% filter(my_code <=1)
#outcome <- as.numeric(training_df$my_code)
#training_df <- training_df %>% select(-c(my_code,document))
#colnames(X_train) <-  paste0('x',seq(1,length(X_train),1))

table(outcome, useNA = "ifany")
#extract outcome variable from dataframe
ones <- which(outcome==1)
zeros <- which(outcome==0)
all_positions <- c(ones,zeros)

set.seed(123)
sample_zeros <- sample(zeros,4150)
set.seed(123)
sample_ones <- sample(ones,2150)
training_positions <- c(sample_ones,sample_zeros)

X_train <- training_df[training_positions,1:100]
Y_train <- outcome[training_positions]
message('training sets created')
hold_out_positions <- all_positions[!(all_positions %in% training_positions)]
hold_out_df <- training_df[hold_out_positions,]
save(hold_out_df,file="hold_out_df_R2_SVM.rda")
message('hold out df saved')
#training_df <- training_df %>% select(my_code)
#--------------------------------------

# Review the outcome variable distribution.
table(Y_train, useNA = "ifany")

#------------------ CREATE COSTUM  LEARNERS ----------------------------------

floor(sqrt(ncol(X_train)))
mtry_seq = floor(sqrt(ncol(X_train)) * c(0.5, 2))

learners = new.env()

forest_leafs = create.Learner("SL.randomForest",env = learners,
                              detailed_names = T, tune = list(mtry = mtry_seq))

forest_trees = create.Learner("SL.randomForest",env = learners,
                              detailed_names = T, tune = list(ntree = c(3000,2000,1000)))

enet = create.Learner("SL.glmnet", env = learners, detailed_names = T,
                             tune = list(alpha = c(.25,.5,.75,1)))

tune = list(ntrees = c(300,600),
            max_depth = 1:3,
            shrinkage = c(0.01, 0.1))

# Set detailed names = T in order to see the configuration for each function.
# Also shorten the name prefix.
XG_boosters = create.Learner("SL.xgboost",env = learners,
                             tune = tune, detailed_names = T, name_prefix = "xgb")

SVM = create.Learner("SL.svm",env = learners,
                     detailed_names = T, name_prefix = "SVM")
NB = create.Learner("SL.bayesglm",env = learners,
                    detailed_names = T, name_prefix = "Bayes")

# Setup parallel computation 
num_cores = RhpcBLASctl::get_num_cores()
options(mc.cores = 3)
getOption("mc.cores")

# set a different type of seed that works across cores.
# Otherwise the other cores will go rogue won't get repeatable results.
set.seed(1, "L'Ecuyer-CMRG")


system.time({
   cv_slV10_default = CV.SuperLearner(Y = Y_train,
                                      X = X_train,
                                      family = binomial(),
                                      V = 10,
                                      parallel = "multicore",
                                      SL.library = c("SL.mean",
                                                     "SL.SVM",
                                                     enet$names,
                                                     forest_trees$names,
                                                     forest_leafs$names,
                                                     XG_boosters$names),
                                      env=learners,
                                      control = list(saveFitLibrary = TRUE),
                                      verbose = TRUE)

 })

 summary(cv_slV10_default)
 save(cv_slV10_default,file='cv_slV10_default_R2.rda')


system.time({
  cv_slV10_AUC_SVM = CV.SuperLearner(Y = Y_train,
                                 X = X_train,
                                 family = binomial(),
                                 V = 10,
                                 parallel = "multicore",
                                 method="method.AUC",
                                 SL.library = c("SL.mean",
                                                SVM$names,
                                                forest_trees$names,
                                                forest_leafs$names,
                                                XG_boosters$names),
                                     env=learners,
                                 control = list(saveFitLibrary = TRUE),
                                 verbose=TRUE)
  
})


save(cv_slV10_AUC_SVM,file="cv_slV10_AUC_R3_SVM.rda")
# Review results.
summary(cv_slV10_AUC)


