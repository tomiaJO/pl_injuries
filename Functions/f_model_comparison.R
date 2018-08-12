f_model_comparison <- function (models, df_test) {
  
  pred_prob  <- predict.train(models[[1]], newdata = df_test, type = "prob")[, "Yes", drop = F]
  names(pred_prob)[1] = names(models)[1]
  
  for(i in 2:length(models)) {
    pred_prob  <- cbind(pred_prob, predict.train(models[[i]], newdata = df_test, type = "prob")[, "Yes", drop = F])
    names(pred_prob)[i] = names(models)[[i]]
  }
  
  p <- f_roc_create(pp = pred_prob, truth = df_test$injured)
  a <- f_calc_auc(  pp = pred_prob, truth = df_test$injured)
  
  return(list("plot" = p, "auc" = a))
}