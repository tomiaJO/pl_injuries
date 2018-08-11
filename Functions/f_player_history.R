f_player_history <- function(df, days_back = Inf) {
  
  game_days <- injuries$Date %>% unique() %>% sort()
  
  injury_history <- vector("list", length(game_days))
  
  i = 1
  for(d in game_days) {
    injury_history[[i]] <- df %>%
                            filter(Date < d & Date >= (d - days_back)) %>%
                            group_by(pid) %>%
                            summarize(injured_before = max(injured),
                                      games_before   = n()) %>%
                            ungroup() %>%
                            mutate(Date = d)
    
    i = i + 1
  }
  
  injury_history   <- do.call("rbind", injury_history)
  class(injury_history$Date) <- "Date"
  
  return(injury_history)
}