##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

## Load data_train & data_test
data_train     <- readRDS(file = paste(path_Data, "data_train.RDS",       sep = "/"))
data_test      <- readRDS(file = paste(path_Data, "data_test.RDS",        sep = "/"))

## Load models
## GLM - Logit
# m_glm_no       <- readRDS(file = paste(path_Models, "m_glm_no.RDS",       sep = "/"))
# m_glm_up       <- readRDS(file = paste(path_Models, "m_glm_up.RDS",       sep = "/"))
m_glm_down     <- readRDS(file = paste(path_Models, "m_glm_down.RDS",     sep = "/"))
# m_glm_smote    <- readRDS(file = paste(path_Models, "m_glm_smote.RDS",    sep = "/"))

## GLM - Probit
m_probit_down  <- readRDS(file = paste(path_Models, "m_probit_down.RDS",     sep = "/"))

## GLMNet
m_glmnet_down  <- readRDS(file = paste(path_Models, "m_glmnet_down.RDS",  sep = "/"))

## RF
m_rf_down      <- readRDS(file = paste(path_Models, "m_rf_down.RDS",      sep = "/"))

## GBM --> 3.5 hours
#m_gbm_down     <- readRDS(file = paste(path_Models, "m_gbm_down.RDS",     sep = "/"))

## XGBOOST
m_xgboost_down  <- readRDS(file = paste(path_Models, "m_xgboost_down.RDS", sep = "/"))


#####################################
## MODEL PERFORMANCE

## Resampling
model_list <- list("Logit"         = m_glm_down,
                   "Probit"        = m_probit_down,
                   "GLMNet"        = m_glmnet_down,
                   "Random Forest" = m_rf_down,                   
                   "XGBoost"       = m_xgboost_down
                   )

resamples_object <- resamples(model_list)

p_resampling <- resamples_object$values %>%
                          tidyr::gather(key= "Resample", factor_key = F) %>%
                          data.table::setnames(c("Model~Metric", "Value")) %>%
                          mutate(model  = stringr::str_split(`Model~Metric`, "~", simplify = T)[,1],
                                 metric = stringr::str_split(`Model~Metric`, "~", simplify = T)[,2]) %>%
                          mutate(metric = factor(metric,
                                                 levels = c("ROC",       "Sens",              "Spec"),
                                                 labels = c("AUC (ROC)", "Sensitivity (TPR)", "Specificy (TNR)"))) %>%
                          mutate(model = factor(model, levels = names(model_list))) %>%
                          ggplot(aes(x = model, y = Value, fill = model)) +
                          geom_boxplot() +
                          facet_wrap(~metric, ncol = 1) +
                          labs(title    = "Comparison of model performances",
                               subtitle = "Logit, Probit, GLMNet, Random Forest and XGBoost",
                               x        = "Model",
                               y        = "",
                               caption  = "Note: Results are based on 50 resamples of training data") +
                          theme_minimal() +
                          theme(plot.title  =  element_text(size = 20, face = "italic"),
                                axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                                axis.text.y = element_text(size = 12))

ggsave(filename = paste(path_Figures, "9999. Model performance - resampling.jpeg", sep = "/"), 
       plot = p_resampling, 
       device = "jpeg", 
       width = 7.5, height = 12,
       dpi = 400)


test_eval <- f_model_comparison(models = model_list, df_test = data_test)

test_eval$auc

p_roc <- test_eval$plot +
            labs(title = "ROC Curves for different models",
                 subtitle = "Logit, Probit, GLMNet, Random Forest and XGBoost",
                 caption = "Note: Results are measured on a held-out test set not used in model fitting")

ggsave(filename = paste(path_Figures, "9999. ROC Curve achieved by different models.jpeg", sep = "/"), 
       plot = p_roc, 
       device = "jpeg", 
       width = 6.5, height = 4,
       dpi = 400)


####################################
## Variable Importance
vi_xgboost_down <- varImp(m_xgboost_down, scale = TRUE)$importance %>% 
                     mutate("Variable" = row.names(.)) %>%
                     rename("XGBoost" = "Overall") %>%
                     select(Variable, XGBoost)

vi_rf_down      <- varImp(m_rf_down, scale = TRUE)$importance %>% 
                      mutate("Variable" = row.names(.)) %>%
                      rename("Random Forest" = "Overall")

vi_glmnet_down  <- varImp(m_glmnet_down, scale = TRUE)$importance %>% 
                     mutate("Variable" = row.names(.)) %>%
                     rename("GLMNet" = "Overall")

vi_probit_down  <- varImp(m_probit_down, scale = TRUE)$importance %>% 
                     mutate("Variable" = row.names(.)) %>%
                     rename("Probit" = "Overall")

vi_logit_down   <- varImp(m_glm_down, scale = TRUE)$importance %>% 
                     mutate("Variable" = row.names(.)) %>%
                     rename("Logit" = "Overall")

variable_importance <- vi_xgboost_down %>%
                        left_join(vi_rf_down,      by = c("Variable" = "Variable")) %>%
                        left_join(vi_glmnet_down,  by = c("Variable" = "Variable"))
                        # left_join(vi_probit_down,  by = c("Variable" = "Variable")) %>%
                        # left_join(vi_logit_down,   by = c("Variable" = "Variable")) %>%

variable_importance <- cbind(variable_importance, `Avg. Importance` = rowMeans(variable_importance[, 2:4], na.rm=TRUE))
variable_importance <- variable_importance %>%
                        mutate(Variable = reorder(Variable, `XGBoost`))

p_vi <- variable_importance %>%
          top_n(15, `Avg. Importance`) %>%
          select(-`Avg. Importance`) %>%
          tidyr::gather(key = "Model", value = "Importance", -Variable) %>%
          mutate(Importance = ifelse(is.na(Importance), 0, Importance)) %>%
          ggplot(aes(x = Variable, y = Importance, color = Model)) +
            geom_line(aes(group = Variable), color = "grey", size = 1.5) +  
            geom_point(size = 3.5) +
            theme_minimal() +
            coord_flip() +
            labs(title = "Variable Importances",
                 subtitle = "GLMNet, Random Forest, XGBoost",
                 caption = "Note: Only top 15 most important variables (on avg.) are shown")

ggsave(filename = paste(path_Figures, "9999. Variable Importance.jpeg", sep = "/"), 
       plot = p_vi, 
       device = "jpeg", 
       width = 6, height = 4,
       dpi = 400)
