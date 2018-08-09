f_breakdown_w_ci <- function(df, s_title = NULL, s_subtitle = NULL, s_caption = NULL, s_x) {
  
    ggplot(data = df, aes_string(x = s_x, group = 1)) +
    geom_line(aes(y = injury_rate_pct)) +
    geom_errorbar(aes(ymin = ci95_lower_injury_rate_pct, ymax = ci95_upper_injury_rate_pct), 
                  colour = "black", 
                  width = .4) +
    geom_point(aes(y = injury_rate_pct, size = game_count), shape = 21, fill = "white") + 
    expand_limits(y = 0) +
    labs(y = "Injury rate (in %)", 
         x = s_x, 
         title = s_title, 
         subtitle = s_subtitle,
         caption = s_caption)
}