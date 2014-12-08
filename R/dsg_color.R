library(grDevices)

# Since there is no significant difference between photocopy_safe and color_blind_safe when
# you print it out using a grey-scale printer, I set .color_blind_safe_color_scheme as default
.photocopy_safe_color_scheme <-  c("#D7191C", "#2B83BA", "#FDAE61", "#FFFFBF", "#ABDDA4")
.color_blind_safe_color_scheme <- c("#D7191C", "#2C7BB6", "#FDAE61", "#FFFFBF", "#ABD9E9")
.matlab_color_scheme <- c("#0000FF", "#FF0000", "#FF00FF", "#000000", "#FF00FF", "#FF00FF")

#' Return color schemes. 
#' 
#' If there are more than 5 lines in a graph, you probably need to reconsider about drawing it. If you insist, all I can give is grey scale colors.
#' @param data Data frame, if NULL then ignore
#' @param measurevar Use this to measure unique values in data frame to determine the number of colors we return
#' @param num Number of colors you want, if specified, then data and measurevar are ignored 
#' @param prefer Preference of colors, color_blind_safe or photocopy_safe
#' 
#' @examples
#' dsg_color(num=1)
#' dsg_color(data=data.frame(x=c(1,2), y=c(3,4), z=c(5,6)), measurevar="z")
#' 
#' @export
dsg_color <- function(data=NULL, measurevar=NULL, num=NULL, prefer="color_blind_safe") {
  if(prefer == "color_blind_safe") {
    color_scheme <- .color_blind_safe_color_scheme
  } else if(prefer == "photo_copy_safe") {
    color_scheme <- .photocopy_safe_color_scheme
  } else {
    color_scheme <- .matlab_color_scheme
  }
  if (is.null(num)) {
    if(is.null(data) || is.null(measurevar)) {
      return(NULL)
    } else {
      num <- length(unique(data[, measurevar]))
    }
  }
  
  if(num <= 5) {
    return (head(color_scheme,num))
  } else {
    return(grey.colors(num))
  }
}