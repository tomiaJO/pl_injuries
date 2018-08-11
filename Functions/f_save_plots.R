f_save_plots <- function(save_plots, p, n, title, w, h) {
  
  f_conditional_ggsave(save = save_plots, 
                       p = p$storyplot, 
                       filepath = paste(path_Figures, "/", n, ". ", title, ".jpeg", sep = ""), 
                       w = w, 
                       h = h)
  
  f_conditional_ggsave(save = save_plots, 
                       p = p$ciplot, 
                       filepath = paste(path_Figures, "/", n, "t. ", title, ".jpeg", sep = ""), 
                       w = w, 
                       h = h)
  
  title <- gsub(pattern = "rate", replacement = "length", x = title, ignore.case = T)
  f_conditional_ggsave(save = save_plots, 
                       p = p$ilenplot, 
                       filepath = paste(path_Figures, "/", n, ". ", title, ".jpeg", sep = ""), 
                       w = w, 
                       h = h)
}