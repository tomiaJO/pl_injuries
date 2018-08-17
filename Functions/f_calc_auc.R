f_calc_auc <- function(pp, truth) {
  
  AUC <- vector(mode = "list", length = length(pp))
  
  for (col in 1:length(pp)) {
    rocr_prediction <- ROCR::prediction(pp[, names(pp)[col]], truth)
    AUC[[col]] <- data.frame("Model" = names(pp)[col],
                             "AUC"   = ROCR::performance(rocr_prediction, "auc")@y.values[[1]])
  }
  
  AUC <- do.call("rbind", AUC)
  
  return(AUC)
}

?ROCR::performance
