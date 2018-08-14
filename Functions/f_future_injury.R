f_future_injury <- function(df, days_forward = 0) {
  
  game_days <- injuries$Date %>% unique() %>% sort()
  
  injury_future <- vector("list", length(game_days))
  
  i = 1
  for(d in game_days) {
    injury_future[[i]] <- df %>%
                            filter(Date >= d & Date <= (d + days_forward)) %>%
                            group_by(pid) %>%
                            summarize(injured_future = max(injured)) %>%
                            ungroup() %>%
                            mutate(Date = d)
    
    i = i + 1
  }
  
  injury_future   <- do.call("rbind", injury_future)
  class(injury_future$Date) <- "Date"
  
  return(injury_future)
}