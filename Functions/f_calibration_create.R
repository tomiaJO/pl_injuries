fun_calibration_create <- function(prediction_probs, truth, model= NULL) {
  
  truth_numeric <- ifelse(truth == "Yes", 1, 0)
  score <- prediction_probs$Yes
  
  actual_vs_predicted <- data.table(actual = truth_numeric,
                                    predicted = score)
  
  actual_vs_predicted[, score_category := cut(predicted,
                                              seq(0, .25, 0.005),
                                              include.lowest = TRUE)]
  
  calibration <- actual_vs_predicted[, .(mean_actual = mean(actual),
                                         mean_predicted = mean(predicted),
                                         num_obs = .N),
                                     keyby = .(score_category)]
  
  s_title <- ifelse(is.null(model), "", paste("Calibration plot for Model:", model, sep = " "))
  
  ggplot(calibration,
         aes(x = mean_actual, y = mean_predicted, size = num_obs)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ylim(0, 0.25) + xlim(0, 0.25) +
    labs(title = s_title)
}