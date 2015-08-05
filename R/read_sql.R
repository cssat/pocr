#' Function for reading in a SQL query.
#' 
#' This probably still needs some work.
#' 
#' @param filename You know what goes here
#' @param silent boolean on whether you want warnings ignored (TRUE).
#' @export
read_sql <- function(filename, silent = TRUE) {
    q <- readLines(filename, warn = !silent)
    q <- q[!grepl(pattern = "^\\s*--", x = q)] # remove full-line comments
    q <- sub(pattern = "--.*", replacement = "", x = q) # remove midline comments
    q <- paste(q, collapse = " ")
    return(q)
}