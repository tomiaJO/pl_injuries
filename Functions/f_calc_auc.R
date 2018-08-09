f_calc_auc <- function(prediction_probs, truth) {
  
  rocr_prediction <- ROCR::prediction(prediction_probs$Yes, truth)
  AUC <- ROCR::performance(rocr_prediction, "auc")@y.values[[1]]
  
  return(AUC)
}