#' Generate complex label for plot
#'
#' Currently this can only take one parameter
#' 
#' @param x A list of data you want to use to construct the label
#' @param formula Formula passing to bquote, usually a paste function
#' 
#' data <- c(1,2,3,4,5)
#' label_function(data, paste(gamma, "=", .(x))
#' @export
label_function <- function(x, ...) {
  return(lapply(x, function(x) bquote(...)))
}