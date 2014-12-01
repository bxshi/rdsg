#' Generate complex label for plot
#'
#' TODO: Make formula `...` more easy to write. 
#' 
#' @param x A list/datafarme you want use to construct the label
#' @param formula Formula passing to bquote, usually a paste function
#' 
#' @examples
#' data <- c(1,2,3,4,5)
#' label_function(data, paste(gamma, "=", .(x)))
#' dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6))
#' label_function(dat, paste(alpha,"=", .(x[1]), beta, "=", .(x[2]), gamma, "=",.(x[3])))
#' 
#' @export
label_function <- function(x, ...) {
  if(is.data.frame(x)) {
    return(apply(x, 1, function(x) bquote(...)))
  } else {
    # Not data.frame, use old methods, this only support 1 parameter
    return(lapply(x, function(x) bquote(...)))
  }
}
