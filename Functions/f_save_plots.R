f_save_plots <- function(save_plots, p, n, title, w, h, save_all = FALSE) {
  
  f_conditional_ggsave(save = save_plots, 
                       p = p$grid, 
                       filepath = paste(path_Figures, "/", n, "g. ", title, ".jpeg", sep = ""), 
                       w = w * 1.75, 
                       h = h)
  
  if(save_all) {
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
    
    f_conditional_ggsave(save = save_plots, 
                         p = p$ilenplot, 
                         filepath = paste(path_Figures, "/", n, ". ", title, ".jpeg", sep = ""), 
                         w = w, 
                         h = h)
  }
}