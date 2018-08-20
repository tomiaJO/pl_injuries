##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

## Load data
data_test  <- readRDS(file = paste(path_Data,   "data_perfomance.RDS",      sep = "/"))

## Load model
model      <- readRDS(file = paste(path_Models, "m_rf_smote.RDS",      sep = "/"))

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

p <- pred_vs_thr %>%
  mutate(`Prediction based missed time` = ifelse(Predictions == "Yes", 3.5, ifelse(Actual == "Yes", Length, 0))) %>%
  group_by(Threshold) %>%
  summarize(`Prediction based missed time` = sum(`Prediction based missed time`),
            `Actual missed time`           = sum(Length)) %>% 
  tidyr::gather(key = "Method", value = "Value", -Threshold) %>%
  ggplot(aes(x = Threshold, y = Value, color = Method)) +
    geom_line(size = 2) +
    scale_x_continuous(limits = c(0.475, 0.725)) +
    scale_y_continuous(limits = c(3000, 4500), labels = scales::comma) +
    scale_colour_manual(values = c("firebrick", "steelblue2")) +
    labs(title = "Comparison of prediction based resting vs actual missed time",
         subtitle = "Calculated on set-aside 2017 data",
         y = "Missed time",
         x = "Resting cut-off",
         caption = "Note: If a player is rested, it is assumed he misses 3.5 days.\nOtherwise his actual missed time is counted") +
    technical_theme() +
    theme(legend.title = element_blank(),
          legend.position = "top",
          axis.text.x = element_blank())

ggsave(filename = paste(path_Figures, "xxx. Cost-benefit.jpeg", sep = "/"),
       plot = p, 
       device = "jpeg", 
       width = 5, height = 4,
       dpi = 650)

pred_vs_thr %>% filter(Actual == "Yes") %>% summarize(mean = mean(Length))

pred_class <- ifelse(pred_prob$`Predicted Probability` > 0.500, "Yes", "No")
pred_class <- factor(pred_class, levels = c("Yes", "No"))
saveRDS(confusionMatrix(data = pred_class, reference = pred_prob$injured)$table,
        file = paste(path_Data, "data_cm.RDS", sep = "/"))

