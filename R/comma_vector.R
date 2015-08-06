#' Takes a vector and returns a comma-space separated string for printing.
#' 
#' @param x vector to paste together
#' @param and boolean, whether an "and" should be inserted before the last 
#' element 
#' @param oxford boolean, whether to include an "Oxford comma" (before the 
#' "and")
#' 
#' @export
comma_vector <- function(x, and = TRUE, oxford = TRUE) {
    n <- length(x)
    if (n == 1) return (x)
    if (and) {
        if (n == 2) return (paste(x[1], "and", x[2]))
        return (paste0(paste(x[1:(n-1)], collapse = ", "), 
                       ifelse(oxford, ", ", " "), 
                       "and ", x[n]))
    }
    return (paste(x, collapse = ", "))
}