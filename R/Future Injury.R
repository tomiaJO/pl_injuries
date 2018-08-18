##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

## set parallel processing
cl <- makePSOCKcluster(2)
registerDoParallel(cl)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in prepared training & test sets
data_train    <- readRDS(paste(path_Data, "data_train.RDS",    sep = "/"))
data_train_7  <- readRDS(paste(path_Data, "data_train_7.RDS",  sep = "/"))
data_train_14 <- readRDS(paste(path_Data, "data_train_14.RDS", sep = "/"))
data_train_21 <- readRDS(paste(path_Data, "data_train_21.RDS", sep = "/"))
data_train_28 <- readRDS(paste(path_Data, "data_train_28.RDS", sep = "/"))

data_test     <- readRDS(paste(path_Data, "data_test.RDS",     sep = "/"))
data_test_7   <- readRDS(paste(path_Data, "data_test_7.RDS",   sep = "/"))
data_test_14  <- readRDS(paste(path_Data, "data_test_14.RDS",  sep = "/"))
data_test_21  <- readRDS(paste(path_Data, "data_test_21.RDS",  sep = "/"))
data_test_28  <- readRDS(paste(path_Data, "data_test_28.RDS",  sep = "/"))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Control function for caret
ctrl <- trainControl(method = "cv",
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tuning grid for caret
tg_rf <- expand.grid(.mtry = c(4:16),
                     .splitrule = "gini",
                     .min.node.size = c(25, 50, 75, 100, 125))

Sys.time()

## injured
## RF
# set.seed(93)
# m_rf_0 <- train(x = data_train[, names(data_train) != "injured"],
#                 y = data_train$injured,       
#                 method     = "ranger",
#                 metric     = "ROC",
#                 trControl  = ctrl,
#                 tuneGrid   = tg_rf,
#                 num.trees = 1000,
#                 importance = "impurity")
# 
# saveRDS(object = m_rf_0, file = paste(path_Models, "m_rf_0.RDS", sep = "/"))
# rm(m_rf_0)
# Sys.time()

## 7 days
# set.seed(93)
# m_rf_7 <- train(x = data_train_7[, names(data_train_7) != "injured_7days"],
#                 y = data_train_7$injured_7days,       
#                 method     = "ranger",
#                 metric     = "ROC",
#                 trControl  = ctrl,
#                 tuneGrid   = tg_rf,
#                 num.trees = 1000,
#                 importance = "impurity")
# 
# saveRDS(object = m_rf_7, file = paste(path_Models, "m_rf_7.RDS", sep = "/"))
# rm(m_rf_7)
# Sys.time()

## 14 days
# set.seed(93)
# m_rf_14 <- train(x = data_train_14[, names(data_train_14) != "injured_14days"],
#                  y = data_train_14$injured_14days,        
#                  method     = "ranger",
#                  metric     = "ROC",
#                  trControl  = ctrl,
#                  tuneGrid   = tg_rf,
#                  num.trees = 1000,
#                  importance = "impurity")
# 
# saveRDS(object = m_rf_14, file = paste(path_Models, "m_rf_14.RDS", sep = "/"))
# rm(m_rf_14)
# Sys.time()

## 21 days
# set.seed(93)
# m_rf_21 <- train(x = data_train_21[, names(data_train_21) != "injured_21days"],
#                  y = data_train_21$injured_21days,        
#                  method     = "ranger",
#                  metric     = "ROC",
#                  trControl  = ctrl,
#                  tuneGrid   = tg_rf,
#                  num.trees = 1000,
#                  importance = "impurity")
# 
# saveRDS(object = m_rf_21, file = paste(path_Models, "m_rf_21.RDS", sep = "/"))
# rm(m_rf_21)
# Sys.time()

## 28 days
# set.seed(93)
# m_rf_28 <- train(x = data_train_28[, names(data_train_28) != "injured_28days"],
#                  y = data_train_28$injured_28days,         
#                  method     = "ranger",
#                  metric     = "ROC",
#                  trControl  = ctrl,
#                  tuneGrid   = tg_rf,
#                  num.trees = 1000,
#                  importance = "impurity")
# 
# saveRDS(object = m_rf_28, file = paste(path_Models, "m_rf_28.RDS", sep = "/"))
# rm(m_rf_28)
# Sys.time()

stopCluster(cl)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MODEL EVALUATION
m_rf_0   <- readRDS(file = paste(path_Models,  "m_rf_0.RDS",     sep = "/"))
m_rf_7   <- readRDS(file = paste(path_Models,  "m_rf_7.RDS",     sep = "/"))
m_rf_14  <- readRDS(file = paste(path_Models,  "m_rf_14.RDS",    sep = "/"))
m_rf_21  <- readRDS(file = paste(path_Models,  "m_rf_21.RDS",    sep = "/"))
m_rf_28  <- readRDS(file = paste(path_Models,  "m_rf_28.RDS",    sep = "/"))

model_list <- list(
                   "Game Day"     = m_rf_0,
                   "7 days"       = m_rf_7,
                   "14 days"      = m_rf_14,
                   "21 days"      = m_rf_21,
                   "28 days"      = m_rf_28
                   )

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ROC Curves
pp_0  <- data.frame("Game Day" = predict.train(m_rf_0,  newdata = data_test,    type = "prob")$Yes)
pp_7  <- data.frame("7 days"   = predict.train(m_rf_7,  newdata = data_test_7,  type = "prob")$Yes)
pp_14 <- data.frame("14 days"  = predict.train(m_rf_14, newdata = data_test_14, type = "prob")$Yes)
pp_21 <- data.frame("21 days"  = predict.train(m_rf_21, newdata = data_test_21, type = "prob")$Yes)
pp_28 <- data.frame("28 days"  = predict.train(m_rf_28, newdata = data_test_28, type = "prob")$Yes)

roc_list <- list(f_roc_create(pp = pp_0,  truth = data_test$injured,    plot = F),
                 f_roc_create(pp = pp_7,  truth = data_test_7$injured,  plot = F),
                 f_roc_create(pp = pp_14, truth = data_test_14$injured, plot = F),
                 f_roc_create(pp = pp_21, truth = data_test_21$injured, plot = F),
                 f_roc_create(pp = pp_28, truth = data_test_28$injured, plot = F))

roc_df <- do.call("rbind", roc_list)
roc_df <- roc_df %>% 
            mutate(Model = sub(pattern = "X", replacement = "",  x = Model)) %>%
            mutate(Model = sub(pattern = "\\.", replacement = " ", x = Model)) %>%
            rename("Time Window" = "Model") %>%
            mutate(`Time Window` = factor(`Time Window`, levels = names(model_list)))

p_roc <- ggplot(data = roc_df, aes(x = FPR, y = TPR, color = `Time Window`)) +
            geom_line(size = 1.25) +
            geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
            labs(title = "ROC curves for different prediction time windows",
                 x     = "Specificy (TNR)",
                 y     = "Sensitivity (TPR)",
                 caption = "Note: Based on tuned Random Forest models") +
            technical_theme() +
            theme(legend.position = "right",
                  legend.title = element_blank())

ggsave(filename = paste(path_Figures, "99999. ROC curves for different prediction time windows.jpeg", sep = "/"), 
       plot = p_roc, 
       device = "jpeg", 
       width = 6, height = 4,
       dpi = 400)

##TODO: calculate and save AUCs

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Probability calibrations
actual <- data_test$injured

## plot uncalibrated
p_cal_0  <- f_calibration_create(score = pp_0$Game.Day,  truth = data_test$injured)$plot           + labs(subtitle = "Game Day")
p_cal_7  <- f_calibration_create(score = pp_7$X7.days,   truth = data_test_7$injured_7days)$plot   + labs(subtitle = "7 days")
p_cal_14 <- f_calibration_create(score = pp_14$X14.days, truth = data_test_14$injured_14days)$plot + labs(subtitle = "14 days")
p_cal_21 <- f_calibration_create(score = pp_21$X21.days, truth = data_test_21$injured_21days)$plot + labs(subtitle = "21 days")
p_cal_28 <- f_calibration_create(score = pp_28$X28.days, truth = data_test_28$injured_28days)$plot + labs(subtitle = "28 days")

g_uncalibrated <- arrangeGrob(p_cal_0, p_cal_7, p_cal_14, p_cal_21, p_cal_28, ncol = 5,
                              top = grid::textGrob("Uncalibrated probability plots for different time windows", 
                                                   gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                                   hjust = 1.30))

ggsave(filename = paste(path_Figures, "99999. Uncalibrated probability plots for different time windows.jpeg", sep = "/"), 
       plot = g_uncalibrated, 
       device = "jpeg", 
       width = 15, height = 4,
       dpi = 600)

## Calibrate
pp_0_calibrated  <- f_recalibrate(mean(data_test$injured           == "Yes"), mean(pp_0$Game.Day),  pp_0$Game.Day)
pp_7_calibrated  <- f_recalibrate(mean(data_test_7$injured_7days   == "Yes"), mean(pp_7$X7.days),   pp_7$X7.days)
pp_14_calibrated <- f_recalibrate(mean(data_test_14$injured_14days == "Yes"), mean(pp_14$X14.days), pp_14$X14.days)
pp_21_calibrated <- f_recalibrate(mean(data_test_21$injured_21days == "Yes"), mean(pp_21$X21.days), pp_21$X21.days)
pp_28_calibrated <- f_recalibrate(mean(data_test_28$injured_28days == "Yes"), mean(pp_28$X28.days), pp_28$X28.days)

## Plot calibrated
p_cal_0  <- f_calibration_create(score = pp_0_calibrated,  truth = data_test$injured)$plot           + labs(subtitle = "Game Day")
p_cal_7  <- f_calibration_create(score = pp_7_calibrated,  truth = data_test_7$injured_7days)$plot   + labs(subtitle = "7 days")
p_cal_14 <- f_calibration_create(score = pp_14_calibrated, truth = data_test_14$injured_14days)$plot + labs(subtitle = "14 days")
p_cal_21 <- f_calibration_create(score = pp_21_calibrated, truth = data_test_21$injured_21days)$plot + labs(subtitle = "21 days")
p_cal_28 <- f_calibration_create(score = pp_28_calibrated, truth = data_test_28$injured_28days)$plot + labs(subtitle = "28 days")

g_calibrated <- arrangeGrob(p_cal_0, p_cal_7, p_cal_14, p_cal_21, p_cal_28, ncol = 5,
                            top = grid::textGrob("Calibrated probability plots for different time windows", 
                                                 gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                                 hjust = 1.35))

ggsave(filename = paste(path_Figures, "99999. Calibrated probability plots for different time windows.jpeg", sep = "/"), 
       plot = g_calibrated, 
       device = "jpeg", 
       width = 15, height = 4,
       dpi = 600)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Variable Importance
variable_importance <- f_variable_importance(model_list)

p_vi <- variable_importance %>%
          mutate(Variable = gsub(pattern = "`", replace = "", x = Variable)) %>%
          mutate(Importance = ifelse(!is.na(Importance), Importance, 0)) %>%
          tidyr::spread(key = "Model", value = "Importance") %>%
          mutate(Variable = reorder(Variable, `Game Day`)) %>%
          tidyr::gather(key = "Model", value = "Importance", -Variable) %>%
          mutate(Model = factor(Model, levels = names(model_list))) %>%
          group_by(Variable) %>%
          mutate(`Min. Importance` = max(Importance, na.rm = TRUE)) %>%
          ungroup() %>%
          filter(`Min. Importance` >= 25) %>%
          select(-`Min. Importance`) %>%
          ggplot(aes(x = Variable, y = Importance, shape = Model)) +
          geom_line(aes(group = Variable), size = 1.35, alpha = 0.55) +  
          geom_point(size = 3.5, fill = "black") +
          scale_shape_manual(values = c(15:18, 25)) +
          technical_theme() +
          theme(legend.position = "top",
                legend.title = element_blank()) +
          coord_flip() +
          labs(title = "Variable Importance across prediction time windows",
               caption = "Note: Only variables scoring 25 or above at least once are shown")

ggsave(filename = paste(path_Figures, "99999. Variable Importance across prediction time windows.jpeg", sep = "/"), 
       plot = p_vi, 
       device = "jpeg", 
       width = 8, height = 8,
       dpi = 400)
