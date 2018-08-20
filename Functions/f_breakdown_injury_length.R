f_breakdown_injury_length <- function(df, s_x, s_title = NULL) {
  p <- df %>%
        summarize(`Avg. Length` = mean(injury_length)) %>%
        ungroup() %>%
        ggplot(aes_string(x= s_x , y = "`Avg. Length`", group = "injury_type")) +
        geom_point(size = 1.2) +
        geom_line(size = 1.05) +
        labs(title = gsub(pattern = "rate", replacement = "length", x = s_title, ignore.case = T),
             x = s_x) +
        technical_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}