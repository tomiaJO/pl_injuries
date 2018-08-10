##SETUP
library(caret)
library(DMwR) ## used for SMOTE
## library(ROSE) --> rose not working right now..
library(doParallel)


#cleanup environment
rm(list = ls())
gc()


#import folder structure
source("GlobalVariables.R")


## set parallel processing
cl <- makePSOCKcluster(3)
registerDoParallel(cl)


## Read in prepared training & test sets
data_train <- readRDS(paste(path_Data, "data_train.RDS", sep = "/"))


## Control function for caret
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)


start_time <- Sys.time()
######################################################
## NO SUB-SAMPLING
## GLM, no regularization
# set.seed(93)
# m_glm_no <- train(injured ~ .,
#                   data      = data_train,
#                   method    = "glm",
#                   family    = "binomial",
#                   metric    = "ROC",
#                   trControl = ctrl)
# 
# saveRDS(object = m_glm_no, file = paste(path_Models, "m_glm_no.RDS", sep = "/"))
# rm(m_glm_no)


## GLMNET, lasso & ridge
# set.seed(93)
# m_glmnet_no <- train(injured ~ ., 
#                      data       = data_train,
#                      method     = "glmnet",
#                      family     = "binomial",
#                      metric     = "ROC",
#                      trControl  = ctrl,
#                      preProcess = c("center", "scale"), 
#                      tuneLength = 10)
# 
# saveRDS(object = m_glmnet_no, file = paste(path_Models, "m_glmnet_no.RDS", sep = "/"))
# rm(m_glmnet_no)


## RF
# tg_rf <- data.frame(mtry = c(2:7))
# 
# set.seed(93)
# m_rf_no <- train(x = data_train[, names(data_train) != "injured"],
#                     y = data_train$injured,
#                     method     = "rf",
#                     metric     = "ROC",
#                     trControl  = ctrl,
#                     preProcess = c("center", "scale"), 
#                     tuneGrid   = tg_rf,
#                     ntree      = 500,
#                     importance = T)
# 
# saveRDS(object = m_rf_no, file = paste(path_Models, "m_rf_no.RDS", sep = "/"))
# rm(m_rf_no)


######################################################
## DOWN-SAMPLING
## GLM, no regularization
ctrl$sampling = "down"

# set.seed(93)
# m_glm_down <- train(injured ~ .,
#                     data      = data_train,
#                     method    = "glm",
#                     family    = "binomial",
#                     metric    = "ROC",
#                     trControl = ctrl)
# 
# saveRDS(object = m_glm_down, file = paste(path_Models, "m_glm_down.RDS", sep = "/"))
# rm(m_glm_down)

## GLMNET, lasso & ridge
# set.seed(93)
# m_glmnet_down <- train(injured ~ .,
#                        data       = data_train,
#                        method     = "glmnet",
#                        family     = "binomial",
#                        metric     = "ROC",
#                        trControl  = ctrl,
#                        preProcess = c("center", "scale"),
#                        tuneLength = 10)
# 
# saveRDS(object = m_glmnet_down, file = paste(path_Models, "m_glmnet_down.RDS", sep = "/"))
# rm(m_glmnet_down)


## RF
# tg_rf <- data.frame(mtry = c(2:7))
# 
# set.seed(93)
# m_rf_down <- train(x = data_train[, names(data_train) != "injured"],
#                        y = data_train$injured,
#                        method     = "rf",
#                        metric     = "ROC",
#                        trControl  = ctrl,
#                        preProcess = c("center", "scale"),
#                        tuneGrid   = tg_rf,
#                        ntree      = 500,
#                        importance = T)
# 
# saveRDS(object = m_rf_down, file = paste(path_Models, "m_rf_down.RDS", sep = "/"))
# rm(m_rf_down)


## XGBOOST
ctrl$verboseIter = T
tg_xgb <- expand.grid(nrounds = 250, 
                      max_depth = c(2:5),
                      eta = c(0.01, 0.05), 
                      gamma = 0,
                      colsample_bytree = c(.25, .5, .75), 
                      min_child_weight = 1, 
                      subsample = c(.25, .5, .75))

set.seed(93)
m_xgboost_down <- train(injured ~ .,
                    method = "xgbTree",
                    metric = "ROC",
                    data = data_train,
                    trControl = ctrl,
                    tuneGrid = tg_xgb)

saveRDS(object = m_xgboost_down, file = paste(path_Models, "m_xgboost_down.RDS", sep = "/"))
rm(m_xgboost_down)

######################################################
## UP-SAMPLING
## GLM, no regularization
# ctrl$sampling = "up"
# 
# set.seed(93)
# m_glm_up <- train(injured ~ .,
#                   data      = data_train,
#                   method    = "glm",
#                   family    = "binomial",
#                   metric    = "ROC",
#                   trControl = ctrl)
# 
# saveRDS(object = m_glm_up, file = paste(path_Models, "m_glm_up.RDS", sep = "/"))
# rm(m_glm_up)


## GLMNET, lasso & ridge
# set.seed(93)
# m_glmnet_up <- train(injured ~ ., 
#                      data       = data_train,
#                      method     = "glmnet",
#                      family     = "binomial",
#                      metric     = "ROC",
#                      trControl  = ctrl,
#                      preProcess = c("center", "scale"), 
#                      tuneLength = 10)
# 
# saveRDS(object = m_glmnet_up, file = paste(path_Models, "m_glmnet_up.RDS", sep = "/"))
# rm(m_glmnet_up)


## RF
# tg_rf <- data.frame(mtry = c(2:7))
# 
# set.seed(93)
# m_rf_up <- train(x = data_train[, names(data_train) != "injured"],
#                    y = data_train$injured,
#                    method     = "rf",
#                    metric     = "ROC",
#                    trControl  = ctrl,
#                    preProcess = c("center", "scale"), 
#                    tuneGrid   = tg_rf,
#                    ntree      = 500,
#                    importance = T)
# 
# saveRDS(object = m_rf_up, file = paste(path_Models, "m_rf_up.RDS", sep = "/"))
# rm(m_rf_up)


######################################################
## SMOTE
## GLM, no regularization
# ctrl$sampling = "smote"
# 
# set.seed(93)
# m_glm_smote <- train(injured ~ .,
#                      data      = data_train,
#                      method    = "glm",
#                      family    = "binomial",
#                      metric    = "ROC",
#                      trControl = ctrl)
# 
# saveRDS(object = m_glm_smote, file = paste(path_Models, "m_glm_smote.RDS", sep = "/"))
# rm(m_glm_smote)


## GLMNET, lasso & ridge
# set.seed(93)
# m_glmnet_smote <- train(injured ~ ., 
#                         data       = data_train,
#                         method     = "glmnet",
#                         family     = "binomial",
#                         metric     = "ROC",
#                         trControl  = ctrl,
#                         preProcess = c("center", "scale"), 
#                         tuneLength = 10)
# 
# saveRDS(object = m_glmnet_smote, file = paste(path_Models, "m_glmnet_smote.RDS", sep = "/"))
# rm(m_glmnet_smote)


## RF
# tg_rf <- data.frame(mtry = c(2:7))
# 
# set.seed(93)
# m_rf_smote <- train(x = data_train[, names(data_train) != "injured"],
#                  y = data_train$injured,
#                  method     = "rf",
#                  metric     = "ROC",
#                  trControl  = ctrl,
#                  preProcess = c("center", "scale"), 
#                  tuneGrid   = tg_rf,
#                  ntree      = 500,
#                  importance = T)
# 
# saveRDS(object = m_rf_smote, file = paste(path_Models, "m_rf_smote.RDS", sep = "/"))
# rm(m_rf_smote)


end_time <- Sys.time()
stopCluster(cl)




### NOT USED ###
#sessioninfo::session_info()

## Basic glm model, no regularization, ROSE
## note: https://github.com/topepo/caret/issues/145
# ctrl$sampling = "rose"
# 
# set.seed(93)
# m_glm_rose <- train(injured ~ ., 
#                     data = data_train,
#                     method    = "glm",
#                     family    = "binomial",
#                     metric    = "ROC",
#                     trControl = ctrl)
# 
# 
# ## GLMNET, lasso & ridge, ROSE
# set.seed(93)
# m_glmnet_no_rose <- train(injured ~ ., 
#                           data = data_train,
#                           method    = "glmnet",
#                           family    = "binomial",
#                           metric    = "ROC",
#                           trControl = ctrl,
#                           preProcess = c("center", "scale"), 
#                           tuneLength = 10)