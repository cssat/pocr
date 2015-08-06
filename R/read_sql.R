#' Function for reading in a SQL query.
#' 
#' This probably still needs some work.
#' 
#' @param filename You know what goes here
#' @param silent boolean on whether you want warnings ignored (TRUE).
#' @export
read_sql <- function(filename, silent = TRUE) {
    q <- readLines(filename, warn = !silent)
    # remove full-line comments
    q <- q[!grepl(pattern = "^\\s*--", x = q)] 
    # remove midline comments
    q <- sub(pattern = "--.*", replacement = "", x = q) 
    q <- paste(q, collapse = " ")
    return(q)
}