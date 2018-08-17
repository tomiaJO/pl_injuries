##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

## Load data
data_test  <- readRDS(file = paste(path_Data,   "data_test.RDS",      sep = "/"))

## Load model
model      <- readRDS(file = paste(path_Models, "m_rf_down.RDS",      sep = "/"))

#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pred_prob <- predict.train(model, newdata = data_test, type = "prob")

pred_prob <- pred_prob %>% 
              select(Yes) %>% 
              rename("Predicted Probability" = "Yes")

pred_prob <- cbind(pred_prob, data_test)
f_roc_create(pred_prob[, c("Predicted Probability"), drop = F], data_test$injured)
f_calibration_create(score = pred_prob$`Predicted Probability`, truth = data_test$injured)

min_prob <- min(pred_prob$`Predicted Probability`)
max_prob <- max(pred_prob$`Predicted Probability`)


thresholds <- seq(min_prob, max_prob, by = (max_prob - min_prob) / 100)

pred_vs_thr <- vector(mode = "list", length = length(thresholds))

for (ix in 1:length(thresholds)) {
  
  pred <- ifelse(pred_prob$`Predicted Probability` > thresholds[ix], "Yes", "No")
  pred <- factor(pred, levels = c("Yes", "No"))
  
  pred_vs_thr[[ix]] <- data.frame("Threshold"   = thresholds[ix],
                                  "Predictions" = pred,
                                  "Actual"      = pred_prob$injured,
                                  "Length"      = pred_prob$injury_length)
}

pred_vs_thr <- do.call("rbind", pred_vs_thr)

pred_vs_thr %>%
  mutate(`Prediction based missed time` = ifelse(Predictions == "Yes", 3.5, ifelse(Actual == "Yes", Length, 0))) %>%
  group_by(Threshold) %>%
  summarize(`Prediction based missed time` = sum(`Prediction based missed time`),
            `Actual missed time`           = sum(Length)) %>% 
  View()
  tidyr::gather(key = "Method", value = "Value", -Threshold) %>%
  ggplot(aes(x = Threshold, y = Value, color = Method)) +
    geom_line() +
    theme_minimal() +
   # scale_x_continuous(limits = c(0.5, 0.8)) 
   scale_y_continuous(limits = c(0, 9500))

pred_class <- ifelse(pred_prob$`Predicted Probability` > 0.65, "Yes", "No")
pred_class <- factor(pred_class, levels = c("Yes", "No"))
confusionMatrix(data = pred_class, reference = pred_prob$injured) %>% as.matrix()


