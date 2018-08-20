f_calibration_create <- function(score, truth) {
  
  truth_numeric <- ifelse(truth == "Yes", 1, 0)
  
  actual_vs_predicted <- data.frame("actual" = truth_numeric, 
                                    "predicted" = score)
  
  min_prob <- min(score)
  max_prob <- max(score)
  
  actual_vs_predicted <- actual_vs_predicted %>%
                          mutate(score_category = cut(predicted, 
                                                      seq(min_prob, max_prob, (max_prob - min_prob) / 10), 
                                                      include.lowest = TRUE))
  
  calibration <- actual_vs_predicted %>%
                  group_by(score_category) %>%
                  summarize(mean_actual = mean(actual),
                            mean_predicted = mean(predicted),
                            num_obs = n()) %>%
                  ungroup()
  
  max_pred   <- max(calibration$mean_predicted)
  max_actual <- max(calibration$mean_actual)
  
  p <- ggplot(calibration, aes(x = mean_actual, y = mean_predicted, size = num_obs)) +
        geom_point(alpha = 0.8) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        scale_y_continuous(limits = c(0, max(c(max_pred, max_actual)))) +
        scale_x_continuous(limits = c(0, max(c(max_pred, max_actual)))) +
        technical_theme()
  
  return(list("plot" = p, "data" = calibration))
}