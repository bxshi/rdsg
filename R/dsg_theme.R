library(ggplot2)

#' Graph theme for DSG
#' 
#' This function will return a simple, minimized theme.
#' WARNING: Use this function will lose your existing theme settings.
#' @export
dsg_theme <- function() {
  
  # Set basic theme
  return(theme_classic() + 
            
            # Advanced settings
            theme(axis.line = element_line(color = "black"),
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