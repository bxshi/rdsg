library(ggplot2)

#' Graph theme for DSG
#' 
#' This function will return a simple, minimized theme.
#' WARNING: Use this function will lose your existing theme settings.
#' @export
dsg_theme <- function(font_size = 12) {
  
  # Set basic theme
  return(theme_classic(base_size = font_size) + 
            
            # Advanced settings
            theme(axis.line = element_line(color = "black"),
                  # No padding/margin between axes
          
                  # No panel grid
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  # No panel border
                  panel.border = element_blank(),
                  # No background
                  panel.background = element_blank(),
                  # Text settings for scale
                  axis.text = element_text(size=16),
                  # Text settings for axis titles
                  axis.title.y = element_text(size=18),
                  axis.title.x = element_text(size=18),
                  # Legend position
                  legend.position = "bottom",
                  legend.text = element_text(size=16),
                  # No legend title
                  legend.title = element_blank(),
                  # Border
                  panel.border = element_rect(colour = "black", fill=NA)))
}