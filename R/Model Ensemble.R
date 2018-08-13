## Import libraries
library(tidyverse)
library(caret)


## Clean-up environment
rm(list = ls())
gc()

## Import folder structure
source("GlobalVariables.R")

## Import performance evaulation functions
source(paste(path_Functions, "f_calc_auc.R",         sep = "/"))
source(paste(path_Functions, "f_roc_create.R",       sep = "/"))
source(paste(path_Functions, "f_model_comparison.R", sep = "/"))

## Load data_test
data_train     <- readRDS(file = paste(path_Data, "data_train.RDS",       sep = "/"))
data_test      <- readRDS(file = paste(path_Data, "data_test.RDS",        sep = "/"))

## Load models
## GLM
# m_glm_no       <- readRDS(file = paste(path_Models, "m_glm_no.RDS",       sep = "/"))
# m_glm_up       <- readRDS(file = paste(path_Models, "m_glm_up.RDS",       sep = "/"))
m_glm_down     <- readRDS(file = paste(path_Models, "m_glm_down.RDS",     sep = "/"))
# m_glm_smote    <- readRDS(file = paste(path_Models, "m_glm_smote.RDS",    sep = "/"))

## GLMNet
m_glmnet_down  <- readRDS(file = paste(path_Models, "m_glmnet_down.RDS",  sep = "/"))

## RF
m_rf_down      <- readRDS(file = paste(path_Models, "m_rf_down.RDS",      sep = "/"))

## GBM --> 3.5 hours
#m_gbm_down     <- readRDS(file = paste(path_Models, "m_gbm_down.RDS",     sep = "/"))

## XGBOOST
m_xgboost_down  <- readRDS(file = paste(path_Models, "m_xgboost_down.RDS", sep = "/"))


#####################################
## COMBINING MODELS

pred_ensemble_train <- data.frame(
      "injured"         = data_train$injured,
      "glm"             = predict.train(m_glm_down,     newdata = data_train, type = "prob")[, "Yes", drop = T],
      "GLMNet"          = predict.train(m_glmnet_down,  newdata = data_train, type = "prob")[, "Yes", drop = T],
      "Random Forest"   = predict.train(m_rf_down,      newdata = data_train, type = "prob")[, "Yes", drop = T],
      "XGBOOST"         = predict.train(m_xgboost_down, newdata = data_train, type = "prob")[, "Yes", drop = T])

cor(pred_ensemble_train[, c(2:5)])

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "smote")

tg_rf <- expand.grid(.mtry = c(2:4),
                     .splitrule = "gini",
                     .min.node.size = c(5, 10, 25, 100))

set.seed(93)
m_ensemble_down <- train(injured ~ .,
                         data = pred_ensemble_train,
                         method     = "ranger",
                         metric     = "ROC",
                         trControl  = ctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid   = tg_rf,
                         num.trees = 1000,
                         importance = "impurity")


pred_ensemble_test <- data.frame(
  "injured"         = data_test$injured,
  "glm"             = predict.train(m_glm_down,     newdata = data_test, type = "prob")[, "Yes", drop = T],
  "GLMNet"          = predict.train(m_glmnet_down,  newdata = data_test, type = "prob")[, "Yes", drop = T],
  "Random Forest"   = predict.train(m_rf_down,      newdata = data_test, type = "prob")[, "Yes", drop = T],
  "XGBOOST"         = predict.train(m_xgboost_down, newdata = data_test, type = "prob")[, "Yes", drop = T])

pp_ens <- predict.train(m_ensemble_down, newdata = pred_ensemble_test, type = "prob")[, "Yes", drop = T]
pred_ensemble_test$Ensemble <- pp_ens

pred_ensemble_test %>% head()

f_roc_create (pred_ensemble_test[, c(2:6)], pred_ensemble_test$injured)
f_calc_auc(  pred_ensemble_test[, c(2:6)], pred_ensemble_test$injured)


ggsave(filename = "ROC_on_test.jpeg", device = "jpeg", dpi = 1600, width = 6, height = 4)
