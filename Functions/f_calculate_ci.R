f_calculate_ci <- function(p, n, alpha = .95) {
  
  cl = 1 - (1-alpha) / 2
  
  upper <- p + qnorm(cl) * sqrt(p * (1-p) / n)
  lower <- p - qnorm(cl) * sqrt(p * (1-p) / n)
  
  ci = list("upper" = upper, "lower" = lower)
  
  return(ci)
}

##source: https://sigmazone.com/binomial-confidence-intervals/

# ## alternative:
# library(binom)
# binom.confint(33, 6353, conf.level = .95, method = "exact")