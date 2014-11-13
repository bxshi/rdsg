#' Assign color for each group
#' 
#' @export
color <- function(data=NULL, measurevar=NULL, colorvar="color", color_list=NULL) {
  if (is.null(color_list)) {
    color_list <- c("red", "green", "blue", "deeppink", "coral1", "black", "yellow3")
  }
  
  # Get unique vars
  uniq_var <- unique(data[, measurevar])
  
  if(length(uniq_var) > length(color_list)) {
    color_list <- rep(color_list, ceiling(length(uniq_var) / color_list))
  }
  
  colors <- apply(data, 1, function(x) color_list[which(uniq_var == x[which(colnames(data) == measurevar)])])
  return(cbind(colors, data))
}