f_breakdown_by_variable <- function(df, s) {
  df %>%
    group_by(!!as.name(s)) %>%
    summarize(game_count = n(),
              injury_count = (sum(injury_type != 0)),
              injury_rate_pct = injury_count / game_count * 100,
              avg_minutes_played = mean(minutes)) %>%
    ungroup() %>%
    arrange(-injury_rate_pct)
}