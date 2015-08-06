#' Outputs a nice date for inline inclusion (wrapper for strftime)
#' 
#' @param x vector of POSIX objects (or something that can be coerced that way)
#' @param format  character with "m", "d", and "y", in the order you want them.
#' @param abbreviate boolean, whether to abbreviate the day.
#' 
#' @export
pretty_date <- function(x,
                        format = c("mdy", "dmy", "ymd"),
                        abbreviate = FALSE, ...) {
    format <- match.arg(format)
    if (format == "mdy" & !abbreviate) 
        return(strftime(x, format = "%B %d, %Y", ...))
    if (format == "mdy" &  abbreviate) 
        return(strftime(x, format = "%b %d, %Y", ...))
    if (format == "dmy" & !abbreviate)
        return(strftime(x, format = "%d %B %Y", ...))
    if (format == "dmy" &  abbreviate) 
        return(strftime(x, format = "%d %b %Y", ...))
    if (format == "ymd" & !abbreviate) 
        return(strftime(x, format = "%Y %B %d", ...))
    if (format == "ymd" &  abbreviate) 
        return(strftime(x, format = "%Y %b %d", ...))  
}