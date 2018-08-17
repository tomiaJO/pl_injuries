##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

## Load data_test
data_test      <- readRDS(file = paste(path_Data, "data_test.RDS",        sep = "/"))

## Load models
## GLM
model_no       <- readRDS(file = paste(path_Models, "m_rf_no.RDS",        sep = "/"))
model_up       <- readRDS(file = paste(path_Models, "m_rf_up.RDS",        sep = "/"))
model_down     <- readRDS(file = paste(path_Models, "m_rf_down.RDS",      sep = "/"))
model_smote    <- readRDS(file = paste(path_Models, "m_rf_smote.RDS",     sep = "/"))

#####################################
## SUB-SAMPLING STRATEGY COMPARISON
model_list <- list("SMOTE"          = model_smote,
                   "No Sampling"    = model_no,
                   "Up-sampling"    = model_up,
                   "Down-sampling"  = model_down)

test_eval <- f_model_comparison(models = model_list, df_test = data_test)

## AUC
data_subsampling_auc <- test_eval$auc %>%
                          arrange(-AUC)

saveRDS(object = data_subsampling_auc, file = paste(path_Data, "data_subsampling_auc.RDS", sep = "/"))

## ROC Curve
p_roc <- test_eval$plot
p_roc <- p_roc +
          labs(title    = "ROC Curves for different sampling strategies",
               subtitle = "Random Forest, tuned parameters: mtry, min.nod.size",
               caption  = "Note: Results are measured on a held-out test set not used in model fitting") +
          story_theme() +
          theme(legend.position = "right")

## Comments: down-sampling and up-sampling are very similar
## Both in terms of AUC and the actual curve shape
## Will use down-sampling, as it's the best and much more resource-friendly

ggsave(filename = paste(path_Figures, "999. ROC Curve of different sampling strategies.jpeg", sep = "/"), 
       plot = p_roc, 
       device = "jpeg", 
       width = 6, height = 4,
       dpi = 400)

## Probability calibrations
actual <- data_test$injured


## plot uncalibrated
pred_no    <- predict.train(model_no,    newdata = data_test, type = "prob") 
pred_down  <- predict.train(model_down,  newdata = data_test, type = "prob") 
pred_up    <- predict.train(model_up,    newdata = data_test, type = "prob") 
pred_smote <- predict.train(model_smote, newdata = data_test, type = "prob")

p_no       <- f_calibration_create(score = pred_no$Yes, truth = actual)$plot +
                labs(subtitle = "No sampling")
p_down     <- f_calibration_create(score = pred_down$Yes, truth = actual)$plot +
                labs(subtitle = "Down sampling")
p_up       <- f_calibration_create(score = pred_up$Yes, truth = actual)$plot +
                labs(subtitle = "Up sampling")
p_smote    <- f_calibration_create(score = pred_smote$Yes, truth = actual)$plot +
                labs(subtitle = "SMOTE sampling")

g_uncalibrated <- arrangeGrob(p_no, p_down, p_up, p_smote, ncol = 4,
                              top = grid::textGrob("Uncalibrated probability plots for different resampling strategies", 
                                                   gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                                   hjust = 0.95))

ggsave(filename = paste(path_Figures, "999. Uncalibrated probability plots.jpeg", sep = "/"), 
       plot = g_uncalibrated, 
       device = "jpeg", 
       width = 12, height = 3.5,
       dpi = 600)

## Calibrate
f_recalibrate <- function(original_fraction, oversampled_fraction, score) {
  per_p <- 1 + (1 / original_fraction - 1) / (1 / oversampled_fraction - 1) * (1 / score - 1)
  p <- 1 / per_p
    
  return(p)
}

pred_no_calibrated    <- f_recalibrate(mean(actual == "Yes"), mean(pred_no$Yes),    pred_no$Yes)
pred_down_calibrated  <- f_recalibrate(mean(actual == "Yes"), mean(pred_down$Yes),  pred_down$Yes)
pred_up_calibrated    <- f_recalibrate(mean(actual == "Yes"), mean(pred_up$Yes),    pred_up$Yes)
pred_smote_calibrated <- f_recalibrate(mean(actual == "Yes"), mean(pred_smote$Yes), pred_smote$Yes)

## Plot calibrated
p_no       <- f_calibration_create(score = pred_no_calibrated,    truth = actual)$plot + labs(subtitle = "No sampling")
p_down     <- f_calibration_create(score = pred_down_calibrated,  truth = actual)$plot + labs(subtitle = "Down sampling")
p_up       <- f_calibration_create(score = pred_up_calibrated,    truth = actual)$plot + labs(subtitle = "Up sampling")
p_smote    <- f_calibration_create(score = pred_smote_calibrated, truth = actual)$plot + labs(subtitle = "SMOTE sampling")

# f_calc_auc(data.frame("Calibrated " = pred_down_calibrated,
#                       "Original"    = pred_down$Yes), 
#            actual)

g_calibrated <- arrangeGrob(p_no, p_down, p_up, p_smote, ncol = 4,
                            top = grid::textGrob("Calibrated probability plots for different resampling strategies", 
                                                 gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                                 hjust = 0.99))

ggsave(filename = paste(path_Figures, "999. Calibrated probability plots.jpeg", sep = "/"), 
       plot = g_calibrated, 
       device = "jpeg", 
       width = 12, height = 3.5,
       dpi = 600)

## use: https://www3.nd.edu/~rjohns15/content/papers/ssci2015_calibrating.pdf
## https://stats.stackexchange.com/questions/56498/re-scaling-a-confusion-matrix-after-down-sampling-one-class

## Varimp
variable_importance <- f_variable_importance(model_list)

p_vi <- variable_importance %>%
          tidyr::spread(key = "Model", value = "Importance") %>%
          mutate(Variable = reorder(Variable, `Down-sampling`)) %>%
          tidyr::gather(key = "Sampling", value = "Importance", -Variable) %>%
          group_by(Variable) %>%
          mutate(`Min. Importance` = max(Importance)) %>%
          ungroup() %>%
          filter(`Min. Importance` >= 25) %>%
          select(-`Min. Importance`) %>%
          ggplot(aes(x = Variable, y = Importance, shape = Sampling)) +
          geom_line(aes(group = Variable), size = 1.35, alpha = 0.55) +  
          geom_point(size = 3.5) +
          scale_shape_manual(values=c(15, 16, 17, 18)) +
          technical_theme() +
          theme(legend.position = "top",
                legend.title = element_blank()) +
          coord_flip() +
          labs(title = "Variable Importance across sampling techniques",
               caption = "Note: Only variables scoring 25 or above at least once are shown")

ggsave(filename = paste(path_Figures, "999. Variable Importance across sampling techniques.jpeg", sep = "/"), 
       plot = p_vi, 
       device = "jpeg", 
       width = 5.5, height = 6.5,
       dpi = 400)

## comments: wide variety
## SMOTE is usually the outlier