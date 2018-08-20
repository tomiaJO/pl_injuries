f_roc_create <- function(pp, truth, plot = TRUE) {
  
  min_prob <- min(pp)
  max_prob <- max(pp)
  
  thresholds <- seq(min_prob, max_prob, by = (max_prob - min_prob) / 100)
  
  tpr <- rep(0, length(thresholds))
  fpr <- rep(0, length(thresholds))
  
  manual_roc <- vector(mode = "list", length = length(pp))
  
  for (col in 1:length(pp)) {
    for (ix in 1:length(thresholds)) {
      
      pred <- ifelse(pp[, names(pp)[col]] > thresholds[ix], "Yes", "No")
      pred <- factor(pred, levels = c("Yes", "No"))
      
      cm <- as.matrix(confusionMatrix(data = pred, reference = truth))
      
      tpr[ix] <- cm[1, 1] / (cm[1, 1] + cm[2, 1])
      fpr[ix] <- cm[1, 2] / (cm[1, 2] + cm[2, 2])
    }
    
    manual_roc[[col]] <- data.frame("Model"     = names(pp)[col],
                                    "Threshold" = thresholds,
                                    "TPR"       = tpr,
                                    "FPR"       = fpr)
  }
  
  manual_roc <- do.call("rbind", manual_roc)
  
  
  p <- ggplot(data = manual_roc, aes(x = FPR, y = TPR)) +
        geom_line(aes(color = Model), size = 1.25) +
        geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
        labs(title = "ROC curve",
             x     = "1 - Specificity (FPR)",
             y     = "Sensitivity (TPR)") +
        theme_minimal()
  
  if(plot) {
    return(p)
  } else {
    return(manual_roc)
  }
}