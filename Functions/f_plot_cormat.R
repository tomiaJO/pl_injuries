f_plot_cormat <- function(cm) {
  ggplot(data = cm, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "white", high = "firebrick", limit = c(0,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    #technical_theme() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))
}