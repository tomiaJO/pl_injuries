f_fit_rf <- function() {
  train(x = data_train[, names(data_train) != "injured"],
        y = data_train$injured,
        method     = "ranger",
        metric     = "Sens",
        trControl  = ctrl,
        tuneGrid   = tg_rf,
        num.trees  = 1500,
        importance = "impurity")
}