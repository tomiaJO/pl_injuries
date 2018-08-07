f_conditional_ggsave <- function(save = F, p, filepath, w = 6, h = 4) {
  if(save){
    d = w * h * 200 / 3
    
    ggsave(filename = filepath, 
           plot     = p, 
           device   = "jpeg", 
           dpi = d, width = w, height = h)
    
    return(print(paste("Plot saved to:", filepath, sep = "/")))
  }
}