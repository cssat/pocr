#' Factor to numeric via character
#' 
#' @param x factor to be converted to numeric
#' 
#' @export
factor_to_numeric <- function(x) {
    if (! is.factor(x)) warning("factor_to_number: Didn't start as a factor.")
    return(as.numeric(as.character(x)))
}