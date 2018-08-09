f_roc_create <- function(prediction_probs, truth, AUC= NULL, model= NULL) {
  
  min_prob <- min(prediction_probs$Yes)
  max_prob <- max(prediction_probs$Yes)
  
  thresholds <- seq(0, max_prob, by = (max_prob - min_prob) / 100)
  
  true_positive_rates <- rep(0, length(thresholds))
  false_positive_rates <- rep(0, length(thresholds))
  
  for (ix in 1:length(thresholds)) {
    thr <- thresholds[ix]
    test_prediction <- ifelse(prediction_probs$Yes > thr, "Yes", "No")
    test_prediction <- factor(test_prediction, levels = c("Yes", "No"))
    cm <- as.matrix(confusionMatrix(data = test_prediction, reference = truth))
    true_positive_rates[ix] <- cm[1, 1] / (cm[1, 1] + cm[2, 1])
    false_positive_rates[ix] <- cm[1, 2] / (cm[1, 2] + cm[2, 2])
  }
  
  manual_roc <- data.frame("threshold" = thresholds,
                           "true_positive_rate" = true_positive_rates,
                           "false_positive_rate" = false_positive_rates)
  
  s_title <- ifelse(is.null(model), "", paste("ROC curve for Model:", model, sep = " "))
  s_subtitle <- ifelse(is.null(AUC), "", paste("AUC =", round(AUC, 2), sep = " "))
  
  ggplot(data = manual_roc, 
         aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
    labs(title = s_title, subtitle = s_subtitle)
}