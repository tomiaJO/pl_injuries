##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set parallel processing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load training data
data_train <- readRDS(paste(path_Data, "data_train.RDS", sep = "/"))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Control function for caret
ctrl <- trainControl(  method = "cv"
                     , classProbs = TRUE
                     , summaryFunction = twoClassSummary
                     , allowParallel = T
                     )

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## LOGIT
f_fit_logit <- function() {
  train(x = data_train[, names(data_train) != "injured"],
        y = data_train$injured,
        method    = "glm",
        family    = binomial(link = "logit"),
        metric    = "ROC",
        trControl = ctrl)
}

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## PROBIT
f_fit_probit <- function() {
  train(x = data_train[, names(data_train) != "injured"],
        y = data_train$injured,
        method    = "glm",
        family    = binomial(link = "probit"),
        metric    = "ROC",
        trControl = ctrl)
}

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GLMNET
tg_glmnet <- expand.grid(alpha = c(0:30 / 100),
                         lambda = c(0:30 / 100))

f_fit_glmnet <- function() {
  train(x = data_train[, names(data_train) != "injured"],
        y = data_train$injured,
        method     = "glmnet",
        family     = "binomial",
        metric     = "ROC",
        trControl  = ctrl,
        preProcess = c("center", "scale"),
        tuneGrid   = tg_glmnet)
}

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## RF
tg_rf <- expand.grid(.mtry = c(6:16),
                     .splitrule = "gini",
                     .min.node.size = c(1:7 * 25))

f_fit_rf <- function() {
  train(x = data_train[, names(data_train) != "injured"],
        y = data_train$injured,
        method     = "ranger",
        metric     = "Sens",
        trControl  = ctrl,
        tuneGrid   = tg_rf,
        num.trees  = 1500,
        importance = "impurity")
}

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## XGBOOST
tg_xgb <- expand.grid(nrounds = c(250, 500),
                      max_depth = c(5, 10, 15),
                      eta = c(0.1, 0.15, 0.2),
                      gamma = c(0, .25),
                      colsample_bytree = c(5:10 / 10),
                      min_child_weight = c(1, 2, 3),
                      subsample = c(5:10 / 10))

f_fit_xgboost <- function() {
  train(injured ~ .,
     method = "xgbTree",
     metric = "ROC",
     data = data_train,
     trControl = ctrl,
     tuneGrid = tg_xgb)
}
## TO DO: https://machinelearningmastery.com/gentle-introduction-xgboost-applied-machine-learning/

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MODELING FITTING & TUNING

Sys.time()
######################################################
## NO SUB-SAMPLING

## RF
# set.seed(93)
# m_rf_no <- f_fit_rf()
# 
# saveRDS(object = m_rf_no, file = paste(path_Models, "m_rf_no.RDS", sep = "/"))
# rm(m_rf_no)
# Sys.time()


######################################################
## DOWN-SAMPLING
## GLM, no regularization
ctrl$sampling = "down"

set.seed(93)
m_logit_down <- f_fit_logit()

saveRDS(object = m_logit_down, file = paste(path_Models, "m_logit_down.RDS", sep = "/"))
rm(m_logit_down)
Sys.time()


## PROBIT, no regularization
set.seed(93)
m_probit_down <- f_fit_probit()

saveRDS(object = m_probit_down, file = paste(path_Models, "m_probit_down.RDS", sep = "/"))
rm(m_probit_down)
Sys.time()


## GLMNET, lasso & ridge
# set.seed(93)
# m_glmnet_down <- f_fit_glmnet()
# 
# saveRDS(object = m_glmnet_down, file = paste(path_Models, "m_glmnet_down.RDS", sep = "/"))
# rm(m_glmnet_down)
# Sys.time()


## RF
# set.seed(93)
# m_rf_down <- f_fit_rf()
# 
# saveRDS(object = m_rf_down, file = paste(path_Models, "m_rf_down.RDS", sep = "/"))
# rm(m_rf_down)
# Sys.time()


## XGBOOST
# set.seed(93)
# m_xgboost_down <- f_fit_xgboost()
# 
# saveRDS(object = m_xgboost_down, file = paste(path_Models, "m_xgboost_down.RDS", sep = "/"))
# rm(m_xgboost_down)
# Sys.time()

######################################################
## UP-SAMPLING
## GLM, no regularization
ctrl$sampling = "up"

## RF
# set.seed(93)
# m_rf_up <- f_fit_rf()
# 
# saveRDS(object = m_rf_up, file = paste(path_Models, "m_rf_up.RDS", sep = "/"))
# rm(m_rf_up)
# Sys.time()


######################################################
## SMOTE
## GLM, no regularization
ctrl$sampling = "smote"

## RF
# set.seed(93)
# m_rf_smote <- f_fit_rf()
# 
# saveRDS(object = m_rf_smote, file = paste(path_Models, "m_rf_smote.RDS", sep = "/"))
# rm(m_rf_smote)
# Sys.time()


Sys.time()
stopCluster(cl)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Custom summaryFunction
# lostTime <- function (data, lev = NULL, model = NULL) {
#   browser()
#   probs     <- pmax(pmin(as.numeric(data[, "pred"]), 1 - 1e-15), 1e-15)
#   real      <- (as.numeric(data[, "obs"]) - 1)
#   timeloss  <- ifelse(Actual == 1, ifelse(Actual == "Yes", true, false), false)
#   out <- c(mean(real * logPreds + (1 - real) * log1Preds)) * -1
#   names(out) <- c("LogLoss")
#   out
#   
#   mutate(`Prediction based missed time` = ifelse(Predictions == "Yes", 3.5, ifelse(Actual == "Yes", Length, 0))) %>%
#     
# }