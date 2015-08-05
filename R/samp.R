#' Sample of an object.
#' 
#' Shortcut for checking on data.
#' @rdname samp
#' @param x data.frame (or matrix) to sample from
#' @param n number of rows to sample (default is 10)
#' @return n random rows of x
#' @export
samp <- function(x, n) {
    UseMethod("samp")
}

#' @rdname samp
#' @method samp data.frame
#' @export
samp.data.frame <- function(x, n = 10) {
    if (n > nrow(x)) {
        warning("samp: object smaller than sample size, whole object returned.")
        return(x)
    }
    x[sample(nrow(x), size = n), ]
}

#' @rdname samp
#' @method samp matrix
#' @export
samp.matrix <- function(x, n = 10) {
    if (n > nrow(x)) {
        warning("samp: object smaller than sample size, whole object returned.")
        return(x)
    }
    x[sample(nrow(x), size = n), ]
}

#' @rdname samp
#' @method samp default
#' @export
samp.default <- function(x, n = 10) {
    if (n > length(x)) {
        warning("samp: object smaller than sample size, whole object returned.")
        return(x)
    }
    x[sample(length(x), size = n)]
}