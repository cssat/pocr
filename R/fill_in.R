#' Expands a data.frame to include all unique combinations of variables
#' and fills in 0's (or other) for missing values
#' 
#' @param dat data.frame to fill in
#' @param value_name names of columns to be filled in (specify this or 
#' \code{value_index})
#' @param value_index indices of columns to be filled in (specify this or 
#' \code{value_name})
#' @param fill_value how to fill in holes after expansion. (Defaults to 0.)
#' 
#' @export
fill_in <- function(dat, 
                    value_name = NULL, value_index = NULL, fill_value = 0) {
    if (any(is.na(dat))) 
        warning("data.frame for filling ing has NAs which were be replaced.")
    if (is.null(value_index) & (is.null(value_name) | 
                                value_name %nin% names(dat))) {
        stop("Invalid value column specification for filling in.")
    }
    if (is.null(value_index)) 
        {value_index <- which(names(dat) == value_name)}
    
    uniques <- lapply(dat[, -value_index], FUN = unique)
    dat_full <- do.call(expand.grid, args=uniques)
    dat_full <- merge(x = dat_full, y = dat, all.x = TRUE)
    dat_full[is.na(dat_full)] <- 0
    return(dat_full[, names(dat)])
}