###############################################################################
## custom_operators.R

# A collection of custom infix functions useful for POC data work. These
# functions can be used like operators (e.g., 1 + 1).

# For more on infix functions:
# http://adv-r.had.co.nz/Functions.html

###############################################################################

#' Fills in NAs in the first argument from the second
#' 
#' @export
`%coalesce%` <- function(a, b) ifelse(is.na(a), b, a)

#' Tests (element-wise) if the first argument falls within
#' the range of the second.
#' 
#' @examples
#' 1 %btwn% c(0, 5)
#' 0:4 %btwn% c(0.5, 3, 3.5)
#' 
#' @export
`%btwn%` <- function(a, b) a >= min(b) & a <= max(b)

#' 'Not in' - the negation of `%in%`. Returns a vector of length equal to the 
#' left hand side indicating TRUE for the LHS elements that are *not* in the 
#' RHS (and FALSE otherwise). Alternative to the ugly '! vec1 %in% vec2'.
#' 
#' @export
`%nin%` <- function (x, table) match(x, table, nomatch = 0) == 0