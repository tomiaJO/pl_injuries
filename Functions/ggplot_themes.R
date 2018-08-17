technical_theme <- function(){
  theme_minimal() +
    theme(legend.position="none") +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(face = "italic", size = 14),
          plot.subtitle = element_text(color = "gray60", size = 10),
          plot.caption  = element_text(color = "gray60")) +
    theme(axis.title.x = element_text(hjust = 0.36)) +
    theme(axis.title.x =  element_text(hjust = 1, size = 8),
          axis.title.y =  element_text(hjust = 1, size = 8)) +
    theme(text=element_text(family = "Garamond"))
}

story_theme <- function() {
  theme_minimal() +
    theme(legend.position="bottom") +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(face = "italic", size = 14),
          plot.subtitle = element_text(color = "gray60", size = 10),
          plot.caption  = element_text(color = "gray60")) +
    theme(axis.title.x = element_text(hjust = 0.36)) +
    theme(axis.title.x =  element_text(hjust = 1, size = 8),
          axis.title.y =  element_text(hjust = 1, size = 8))
}