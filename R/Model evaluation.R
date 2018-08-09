## Import libraries
library(tidyverse)
library(caret)


## Clean-up environment
rm(list = ls())
gc()

## Import folder structure
source("GlobalVariables.R")

## Import performance evaulation functions
source(paste(path_Functions, "f_calc_auc.R",       sep = "/"))
source(paste(path_Functions, "f_roc_create.R",     sep = "/"))

## Load data_test
data_test      <- readRDS(file = paste(path_Data, "data_test.RDS",        sep = "/"))

## Load models
#m_glm_no       <- readRDS(file = paste(path_Models, "m_glm_no.RDS",       sep = "/"))
m_glmnet_no    <- readRDS(file = paste(path_Models, "m_glmnet_no.RDS",    sep = "/"))

m_glmnet_down  <- readRDS(file = paste(path_Models, "m_glmnet_down.RDS",  sep = "/"))
m_rf_100_down  <- readRDS(file = paste(path_Models, "m_rf_100_down.RDS",  sep = "/"))
m_rf_500_down  <- readRDS(file = paste(path_Models, "m_rf_500_down.RDS",  sep = "/"))
m_rf_1000_down <- readRDS(file = paste(path_Models, "m_rf_1000_down.RDS", sep = "/"))
m_rpart_down   <- readRDS(file = paste(path_Models, "m_rpart_down.RDS",   sep = "/"))

m_glmnet_up    <- readRDS(file = paste(path_Models, "m_glmnet_up.RDS",    sep = "/"))
m_glmnet_smote <- readRDS(file = paste(path_Models, "m_glmnet_smote.RDS", sep = "/"))


#####################################
## MODEL PERFORMANCE

## Resampling
resamples_object <- resamples(list("GLMNet, Down-sampling"  = m_glmnet_down,
                                   "RF 100, Down-sampling"      = m_rf_100_down,
                                   "RF 500, Down-sampling"      = m_rf_500_down,
                                   "RF 1000, Down-sampling"      = m_rf_500_down,
                                   "RPart, Down-sampling"   = m_rpart_down))


p_resampling <- resamples_object$values %>%
                          tidyr::gather(key= "Resample", factor_key = F) %>%
                          data.table::setnames(c("Model~Metric", "Value")) %>%
                          mutate(model  = stringr::str_split(`Model~Metric`, "~", simplify = T)[,1],
                                 metric = stringr::str_split(`Model~Metric`, "~", simplify = T)[,2]) %>%
                          mutate(metric = factor(metric,
                                                 levels = c("ROC",       "Sens",              "Spec"),
                                                 labels = c("AUC (ROC)", "Sensitivity (TPR)", "Specificy (TNR)"))) %>%
                          mutate(model = factor(model, levels = c("GLMNet, Down-sampling",
                                                                  "RPart, Down-sampling",
                                                                  "RF 100, Down-sampling",
                                                                  "RF 500, Down-sampling"))) %>%
                          ggplot(aes(x = model, y = Value, fill = model)) +
                          geom_boxplot() +
                          facet_wrap(~metric, ncol = 1) +
                          labs(title    = "Comparison of model performances",
                               subtitle = "GLMNet, Random Forest, RPart",
                               x        = "Model",
                               y        = "",
                               caption  = "Note: Results are based on 50 resamples of training data") +
                          theme_minimal() +
                          theme(plot.title  =  element_text(size = 20, face = "italic"),
                                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                                axis.text.y = element_text(size = 12))

ggsave(filename = paste(path_Figures, "Model performance - resampling.jpeg", sep = "/"), 
       plot = p_resampling, 
       device = "jpeg", 
       width = 7.5, height = 12,
       dpi = 1600)


## ROC - AUC
predprob_rf_500_down <- predict.train(m_rf_1000_down, newdata = data_test, type = "prob")
test_truth           <- data_test$injured

AUC_rf_500_down <- f_calc_auc(predprob_rf_500_down, test_truth)
ROC_rf_500_down <- f_roc_create(predprob_rf_500_down, test_truth, AUC_rf_500_down, "RF")


## Calibration curve
predprob_rf_100_down %>% glimpse()
## glm_calibration <- fun_calibration_create(glm_prediction_probs, test_truth, "glm")


#####################################
## SUB-SAMPLING STRATEGY COMPARISON
resamples_object <- resamples(list("SMOTE"          = m_glmnet_smote,
                                   "No Sampling"    = m_glmnet_no,
                                   "Up-sampling"    = m_glmnet_up,
                                   "Down-sampling"  = m_glmnet_down))


p_sampling_strategies <- resamples_object$values %>%
                          tidyr::gather(key= "Resample", factor_key = F) %>%
                          data.table::setnames(c("Model~Metric", "Value")) %>%
                          mutate(model  = stringr::str_split(`Model~Metric`, "~", simplify = T)[,1],
                                 metric = stringr::str_split(`Model~Metric`, "~", simplify = T)[,2]) %>%
                          mutate(metric = factor(metric,
                                                 levels = c("ROC",       "Sens",              "Spec"),
                                                 labels = c("AUC (ROC)", "Sensitivity (TPR)", "Specificy (TNR)"))) %>%
                          mutate(model = factor(model, levels = c("No Sampling",
                                                                  "Up-sampling",
                                                                  "Down-sampling",
                                                                  "SMOTE"))) %>%
                          ggplot(aes(x = model, y = Value, fill = model)) +
                          geom_boxplot() +
                          facet_wrap(~metric, ncol = 1) +
                          labs(title    = "Comparison of Sampling strategies",
                               subtitle = "GLMNet, automatic tuning",
                               x        = "Model",
                               y        = "",
                               caption  = "Note: Results are based on 50 resamples of training data") +
                          theme_minimal() +
                          theme(plot.title  =  element_text(size = 20, face = "italic"),
                                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                                axis.text.y = element_text(size = 12))

ggsave(filename = paste(path_Figures, "Comparison of sampling strategies.jpeg", sep = "/"), 
       plot = p_sampling_strategies, 
       device = "jpeg", 
       width = 7.5, height = 12,
       dpi = 1600)
##