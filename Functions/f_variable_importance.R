f_variable_importance <- function(l) {
  
  VI <- vector(mode = "list", length = length(l))
 
  for(i in 1:length(l)) {
    VI[[i]] <- varImp(l[[i]], scale = TRUE)$importance %>% 
                mutate(Variable = row.names(.)) %>%
                rename("Importance" = "Overall") %>%
                mutate(Model = names(l)[[i]])
  }
  
  VI <- do.call("rbind", VI)
  
  return(VI)
  
}