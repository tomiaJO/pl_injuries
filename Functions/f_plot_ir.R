## plot injury rates:
## 1. breakdown by bins --> storytelling
## 2. with confidence intervals --> technical

f_plot_ir <- function(df, s_x, s_facet = NULL, s_title = NULL, s_subtitle = NULL, s_caption = NULL, s_subtitle_ci = NULL, s_caption_ci = NULL) {
  
  if(!is.null(s_facet)) {
    df <- df %>%
            group_by_(s_x, s_facet) 
  } else {
    df <- df %>%
      group_by_(s_x) 
  }
  
  summary <- df %>%
              summarize(game_count = n(),
              injury_count = sum(injured)) %>%
              ungroup() %>%
              mutate(injury_rate_pct = injury_count / game_count) %>%
              mutate(ci95_lower_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$lower,
                     ci95_upper_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$upper) %>%
              mutate(injury_rate_pct            = injury_rate_pct            * 100,
                     ci95_lower_injury_rate_pct = ci95_lower_injury_rate_pct * 100,
                     ci95_upper_injury_rate_pct = ci95_upper_injury_rate_pct * 100)
  
  p_story <- summary %>%
              f_breakdown_by_bins(s_title = s_title,
                                  s_subtitle = s_subtitle,
                                  s_x = s_x) +
              story_theme() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if(!is.null(s_facet)) {
    p_story <- p_story +
                facet_wrap(as.formula(paste("~", s_facet)), ncol = 1)
  }
  
  p_ci <- summary %>%
              f_breakdown_w_ci(s_x        = s_x, 
                               s_title    = s_title, 
                               s_subtitle = s_subtitle_ci,
                               s_caption  = s_caption_ci) +
              technical_theme() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if(!is.null(s_facet)) {
    p_ci <- p_ci +
              facet_wrap(as.formula(paste("~", s_facet)), ncol = 1)
  }
  
  r <- list("storyplot" = p_story,
            "ciplot"    = p_ci,
            "data"      = summary)
  
  return(r)
}
