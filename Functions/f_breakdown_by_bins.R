f_breakdown_by_bins <- function(df, s_x, s_title) {
  max_injury_rate_pct  <- df$injury_rate_pct %>% max()
  max_game_count       <- df$game_count      %>% max()
  
  ggplot(data = df, aes_string(x = s_x)) +
    geom_bar(aes(y = game_count, fill = "# of games"), stat= "identity") +
    geom_point(aes(y = injury_rate_pct * (max_game_count / max_injury_rate_pct), color = "% of injured"), stat= "identity") +
    geom_line( aes(y = injury_rate_pct * (max_game_count / max_injury_rate_pct), color = "% of injured"), stat= "identity", group = 1) +
    scale_y_continuous(sec.axis = sec_axis(~./(max_game_count / max_injury_rate_pct), name = "% of injured", labels = f_sec_scale), 
                       labels = scales::comma) + 
    scale_fill_manual(name = "Legend", values = c("# of games" = "lightblue")) + 
    scale_color_manual(name = element_blank(), values = c("% of injured" = "darkblue")) + 
    labs(y = "# of games", x = s_x, title = s_title) +
    theme_minimal()
  ##TODO: legend clean-up
}