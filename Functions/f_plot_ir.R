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
  
  ## Injury rates & game counts
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
  ##
  
  ## Injury rate w/ CIs
  p_ci <- summary %>%
              f_breakdown_w_ci(s_x        = s_x, 
                               s_title    = s_title, 
                               s_subtitle = s_subtitle_ci,
                               s_caption  = s_caption_ci)
  
  if(!is.null(s_facet)) {
    p_ci <- p_ci +
              facet_wrap(as.formula(paste("~", s_facet)), ncol = 1)
  }
  ##
  
  ## Injury lengths with CIs
  df <- df %>%
          ungroup() %>%
          filter(injured == 1)
  
  if(!is.null(s_facet)) {
    df <- df %>%
      group_by_(s_x, s_facet, "injury_type") 
  } else {
    df <- df %>%
            group_by_(s_x , "injury_type")
  }
  
  p_il <- f_breakdown_injury_length(df, s_x, s_title)
  
  if(!is.null(s_facet)) {
    p_il <- p_il +
      facet_wrap(as.formula(paste("~", s_facet, "+", "injury_type", sep = "")), ncol = 2)
  } else {
    p_il <- p_il +
              facet_wrap(~injury_type)
  }
  ##
  
  ##grid
  p_story <- p_story + theme(plot.title = element_blank())
  p_ci    <- p_ci    + theme(plot.title = element_blank())
  p_il    <- p_il    + theme(plot.title = element_blank())
  g <- arrangeGrob(p_ci,
                   p_il,
                   ncol = 2,
                   # layout_matrix = rbind(rbind(c(1,  1, 1, 2, 2, 2),
                   #                             c(NA, 3, 3, 3, 3, NA))),
                   top = grid::textGrob(s_title, 
                                        gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25)))
  ##
  
  r <- list("storyplot" = p_story,
            "ciplot"    = p_ci,
            "data"      = summary,
            "ilenplot"  = p_il,
            "grid"      = g)
  
  return(r)
}
