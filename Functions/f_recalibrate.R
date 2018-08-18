f_recalibrate <- function(original_fraction, oversampled_fraction, score) {
  per_p <- 1 + (1 / original_fraction - 1) / (1 / oversampled_fraction - 1) * (1 / score - 1)
  p <- 1 / per_p
  
  return(p)
}