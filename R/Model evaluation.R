##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data_train & data_test
# data_train     <- readRDS(file = paste(path_Data, "data_train.RDS",       sep = "/"))
data_test       <- readRDS(file = paste(path_Data,   "data_test.RDS",      sep = "/"))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load models
## GLM - Logit
m_logit_smote    <- readRDS(file = paste(path_Models, "m_logit_smote.RDS",   sep = "/"))

## GLM - Probit
m_probit_smote   <- readRDS(file = paste(path_Models, "m_probit_smote.RDS",  sep = "/"))

## GLMNet
m_glmnet_smote   <- readRDS(file = paste(path_Models, "m_glmnet_smote.RDS",  sep = "/"))

## RF
m_rf_smote       <- readRDS(file = paste(path_Models, "m_rf_smote.RDS",      sep = "/"))

## XGBOOST
m_xgboost_smote  <- readRDS(file = paste(path_Models, "m_xgboost_smote.RDS", sep = "/"))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MODEL PERFORMANCE

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Resampling
model_list <- list(
                   "Logit"         = m_logit_smote,
                   "Probit"        = m_probit_smote,
                   "GLMNet"        = m_glmnet_smote,
                   "Random Forest" = m_rf_smote,
                   "XGBoost"       = m_xgboost_smote
                   )

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ROC Curves and AUC
test_eval <- f_model_comparison(models = model_list, df_test = data_test)

## AUC
data_eval_auc <- test_eval$auc

saveRDS(object = data_eval_auc, file = paste(path_Data, "data_eval_auc.RDS", sep = "/"))

p_roc <- test_eval$plot +
            labs(title    = "ROC Curves for different models",
                 subtitle = "Models were tuned using SMOTE-sampling",
                 caption  = "Note: Results are measured on a held-out test set not used in model fitting") +
            technical_theme() +
            theme(legend.position = "right",
                  legend.title = element_blank())

ggsave(filename = paste(path_Figures, "9999. ROC Curves for different models.jpeg", sep = "/"), 
       plot = p_roc, 
       device = "jpeg", 
       width = 6, height = 4,
       dpi = 400)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Probability calibrations
actual <- data_test$injured

## plot uncalibrated
pred_logit_smote     <- predict.train(m_logit_smote,   newdata = data_test, type = "prob") 
pred_probit_smote    <- predict.train(m_probit_smote,  newdata = data_test, type = "prob") 
pred_glmnet_smote    <- predict.train(m_glmnet_smote,  newdata = data_test, type = "prob") 
pred_rf_smote        <- predict.train(m_rf_smote,      newdata = data_test, type = "prob")
pred_xgboost_smote   <- predict.train(m_xgboost_smote, newdata = data_test, type = "prob")

p_logit_smote   <- f_calibration_create(score = pred_logit_smote$Yes,   truth = actual)$plot + labs(subtitle = "Logit")
p_probit_smote  <- f_calibration_create(score = pred_probit_smote$Yes,  truth = actual)$plot + labs(subtitle = "Probit")
p_glmnet_smote  <- f_calibration_create(score = pred_glmnet_smote$Yes,  truth = actual)$plot + labs(subtitle = "GLMNet")
p_rf_smote      <- f_calibration_create(score = pred_rf_smote$Yes,      truth = actual)$plot + labs(subtitle = "Random Forest")
p_xgboost_smote <- f_calibration_create(score = pred_xgboost_smote$Yes, truth = actual)$plot + labs(subtitle = "XGBoost")

g_uncalibrated <- arrangeGrob(p_logit_smote, p_probit_smote, p_glmnet_smote, p_rf_smote, p_xgboost_smote, ncol = 5,
                              top = grid::textGrob("Uncalibrated probability plots for different models", 
                                                   gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                                   hjust = 1.5))

ggsave(filename = paste(path_Figures, "9999. Uncalibrated probability plots for different models.jpeg", sep = "/"), 
       plot = g_uncalibrated, 
       device = "jpeg", 
       width = 15, height = 4,
       dpi = 600)

## Calibrate
f_recalibrate <- function(original_fraction, oversampled_fraction, score) {
  per_p <- 1 + (1 / original_fraction - 1) / (1 / oversampled_fraction - 1) * (1 / score - 1)
  p <- 1 / per_p
  
  return(p)
}

pred_logit_smote_calibrated   <- f_recalibrate(mean(actual == "Yes"), mean(pred_logit_smote$Yes),   pred_logit_smote$Yes)
pred_probit_smote_calibrated  <- f_recalibrate(mean(actual == "Yes"), mean(pred_probit_smote$Yes),  pred_probit_smote$Yes)
pred_glmnet_smote_calibrated  <- f_recalibrate(mean(actual == "Yes"), mean(pred_glmnet_smote$Yes),  pred_glmnet_smote$Yes)
pred_rf_smote_calibrated      <- f_recalibrate(mean(actual == "Yes"), mean(pred_rf_smote$Yes),      pred_rf_smote$Yes)
pred_xgboost_smote_calibrated <- f_recalibrate(mean(actual == "Yes"), mean(pred_xgboost_smote$Yes), pred_xgboost_smote$Yes)

## Plot calibrated
p_logit_smote   <- f_calibration_create(score = pred_logit_smote_calibrated,   truth = actual)$plot + labs(subtitle = "Logit")
p_probit_smote  <- f_calibration_create(score = pred_probit_smote_calibrated,  truth = actual)$plot + labs(subtitle = "Probit")
p_glmnet_smote  <- f_calibration_create(score = pred_glmnet_smote_calibrated,  truth = actual)$plot + labs(subtitle = "GLMNet")
p_rf_smote      <- f_calibration_create(score = pred_rf_smote_calibrated,      truth = actual)$plot + labs(subtitle = "Random Forest")
p_xgboost_smote <- f_calibration_create(score = pred_xgboost_smote_calibrated, truth = actual)$plot + labs(subtitle = "XGBoost")

g_calibrated <- arrangeGrob(p_logit_smote, p_probit_smote, p_glmnet_smote, p_rf_smote, p_xgboost_smote, ncol = 5,
                            top = grid::textGrob("Calibrated probability plots for different models", 
                                                 gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                                 hjust = 1.5))

ggsave(filename = paste(path_Figures, "9999. Calibrated probability plots.jpeg", sep = "/"), 
       plot = g_calibrated, 
       device = "jpeg", 
       width = 15, height = 4,
       dpi = 600)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Variable Importance
variable_importance <- f_variable_importance(model_list)
## TODO: issues with format...
p_vi <- variable_importance %>%
          mutate(Variable = gsub(pattern = "`", replace = "", x = Variable)) %>%
          tidyr::spread(key = "Model", value = "Importance") %>%
          mutate(Variable = reorder(Variable, `Random Forest`)) %>%
          filter(`Random Forest` >= 25 | GLMNet >= 25) %>%
          tidyr::gather(key = "Model", value = "Importance", -Variable) %>%
          mutate(Model = factor(Model, levels = names(model_list))) %>%
          mutate(Type = ifelse(Model %in% c("Random Forest", "XGBoost"), "Tree-based", "Linear")) %>%
          mutate(Importance = ifelse(!is.na(Importance), Importance, 0)) %>%
          ggplot(aes(x = Variable, y = Importance, shape = Model)) +
          geom_line(aes(group = Variable), size = 1.35, alpha = 0.55) +  
          geom_point(size = 3.5, fill = "black") +
          scale_shape_manual(values = c(15:18, 25)) +
          technical_theme() +
          theme(legend.position = "top",
                legend.title = element_blank()) +
          coord_flip() +
          facet_wrap(~Type) +
          labs(title = "Variable Importance across models",
               caption = "Note: Only variables scoring 25 or above at least once are shown")

ggsave(filename = paste(path_Figures, "9999. Variable Importance across models.jpeg", sep = "/"), 
       plot = p_vi, 
       device = "jpeg", 
       width = 10, height = 8,
       dpi = 400)

coef_glmnet_smote <- coef(m_glmnet_smote$finalModel, m_glmnet_smote$bestTune$lambda)
coef_names_glmnet_smote <- coef_glmnet_smote@Dimnames[[1]][coef_glmnet_smote@i+1]
coef_values_glmnet_smote <- coef_glmnet_smote@x

df_coef_glmnet_smote <- data.frame("Coefficient" = coef_names_glmnet_smote,
                                  "Value" = coef_values_glmnet_smote)

p_coef_glmnet_smote <- df_coef_glmnet_smote %>%
                        mutate(Coefficient = reorder(Coefficient, -Value)) %>%
                        mutate(Value = -1 * Value) %>%
                        mutate(Sign = Value >= 0) %>%
                        filter(abs(Value) >= 0.05) %>%
                        filter(Coefficient != "(Intercept)") %>%
                        ggplot(aes(x = Coefficient, y = Value, fill = Sign)) +
                          geom_bar(stat = "identity") +
                          scale_y_continuous(limits = c(-0.2, 0.2)) +
                          scale_fill_manual(values=c("steelblue2", "firebrick")) +
                          coord_flip() +
                          labs(title = "Coefficent values for GLMNet model",
                               x = "",
                               y = "",
                               caption = "Note: Only variables with relative importance above 0.05 are shown") +
                          story_theme() +
                          theme(legend.position = "none")

ggsave(filename = paste(path_Figures, "9999. Coefficent values for GLMNet models.jpeg", sep = "/"), 
       plot = p_coef_glmnet_smote, 
       device = "jpeg", 
       width = 6, height = 4,
       dpi = 400)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bias
df_bias <- data_frame("injured"       = data_test$injured,
                      "Year"          = data_test$Year,
                      "Logit"         = pred_logit_smote_calibrated,
                      "Probit"        = pred_probit_smote_calibrated,
                      "GLMNet"        = pred_glmnet_smote_calibrated,
                      "Random Forest" = pred_rf_smote_calibrated,
                      "XGBoost"       = pred_xgboost_smote_calibrated)


## By year
df_bias %>% 
  group_by(Year) %>%
  summarize(`Count`         = n(), 
            `Injury Rate`   = sum(injured == "Yes") / n(),
            `Logit`         = mean(Logit),
            `Probit`        = mean(Probit),
            `GLMNet`        = mean(GLMNet),
            `Random Forest` = mean(`Random Forest`),
            `XGBoost`       = mean(`XGBoost`)) %>%
  ungroup() %>%
  select(-Count) %>%
  rename("Actual" = "Injury Rate") %>%
  tidyr::gather(key = "Type", value = "Injury Rate", -Year) %>%
  mutate(Type = factor(Type, levels = c("Actual", "Logit", "Probit", "GLMNet", "Random Forest", "XGBoost"))) %>%
  ggplot(aes(x = Year, y = `Injury Rate`, color = Type)) +
    geom_point(size = 3) +
    geom_line(size = 1.5) +
    scale_y_continuous(limits = c(0, 0.0175)) +
      scale_colour_manual(values = c("firebrick", "grey60", "grey80", "grey20", "steelblue2", "grey40")) +
    labs(title = "Prediction bias over years",
         subtitle = "Average injuries rates") +
    story_theme() +
    theme(legend.title = element_blank())

ggsave(filename = paste(path_Figures, "xxx. Prediction bias over years.jpeg", sep = "/"),
        plot = last_plot(), 
       device = "jpeg", 
       width = 4, height = 6,
       dpi = 400)
