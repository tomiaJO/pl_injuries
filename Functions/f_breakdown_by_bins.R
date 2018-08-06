f_breakdown_by_bins <- function(df, s_title = NULL, s_subtitle = NULL, s_caption = NULL, s_x = NULL) {
  max_injury_rate_pct  <- df$injury_rate_pct %>% max()
  max_game_count       <- df$game_count      %>% max()
  
  ggplot(data = df, aes_string(x = s_x)) +
    geom_bar(aes(y = game_count, fill = "# of games"), stat= "identity") +
    geom_point(aes(y = injury_rate_pct * (max_game_count / max_injury_rate_pct), color = "% of injured"), stat= "identity") +
    geom_line( aes(y = injury_rate_pct * (max_game_count / max_injury_rate_pct), color = "% of injured"), stat= "identity", group = 1) +
    scale_y_continuous(sec.axis = sec_axis(~./(max_game_count / max_injury_rate_pct), name = "% of injured", labels = f_sec_scale), 
                       labels = scales::comma) + 
    scale_fill_manual( name = element_blank(), values = c("# of games"   = "steelblue2"), guide = guide_legend()) + 
    scale_color_manual(name = element_blank(), values = c("% of injured" = "firebrick"),  guide = guide_legend()) + 
    labs(y = "# of games", 
         x = s_x, 
         title = s_title, 
         subtitle = s_subtitle,
         caption = s_caption) +
    theme_minimal() +
    theme(legend.position="bottom") +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(face = "italic", size = 14),
          plot.subtitle = element_text(color = "gray60", size = 10),
          plot.caption  = element_text(color = "gray60")) +
    theme(axis.title.x = element_text(hjust = 0.36)) +
    theme(axis.title.x =  element_text(hjust = 1, size = 8),
          axis.title.y =  element_text(hjust = 1, size = 8))
  ##TODO: legend clean-up
}