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
data_test      <- readRDS(file = paste(path_Data, "data_test.RDS",        sep = "/"))

## Load models
## GLM
m_glm_no       <- readRDS(file = paste(path_Models, "m_glm_no.RDS",       sep = "/"))
m_glm_up       <- readRDS(file = paste(path_Models, "m_glm_up.RDS",       sep = "/"))
m_glm_down     <- readRDS(file = paste(path_Models, "m_glm_down.RDS",     sep = "/"))
m_glm_smote    <- readRDS(file = paste(path_Models, "m_glm_smote.RDS",    sep = "/"))

#####################################
## SUB-SAMPLING STRATEGY COMPARISON
model_list <- list("SMOTE"          = m_glm_smote,
                   "No Sampling"    = m_glm_no,
                   "Up-sampling"    = m_glm_up,
                   "Down-sampling"  = m_glm_down)

resamples_object <- resamples(model_list)


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
       subtitle = "LOGIT, no regularization",
       x        = "Model",
       y        = "",
       caption  = "Note: Results are based on 50 resamples of training data") +
  theme_minimal() +
  theme(plot.title  =  element_text(size = 20, face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12))

ggsave(filename = paste(path_Figures, "999. Comparison of sampling strategies.jpeg", sep = "/"), 
       plot = p_sampling_strategies, 
       device = "jpeg", 
       width = 7.5, height = 12,
       dpi = 400)


test_eval <- f_model_comparison(models = model_list, df_test = data_test)

test_eval$auc

p_roc <- test_eval$plot
p_roc <- p_roc +
          labs(title = "ROC Curves for different sampling strategies",
               subtitle = "LOGIT, no regularization",
               caption = "Note: Results are measured on a held-out test set not used in model fitting")

ggsave(filename = paste(path_Figures, "999. ROC Curve of different sampling strategies.jpeg", sep = "/"), 
       plot = p_roc, 
       device = "jpeg", 
       width = 6.5, height = 4,
       dpi = 400)
