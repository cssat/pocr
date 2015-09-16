#' Validates the json in a config file
#' 
#' This takes a character vector of text from a config file
#' (such as that produced by readLines) and validates the json
#' 
#' We assume that config files
#' \itemize{
#'     \item optionally begin with a single R code chunk on the first line
#'     \item there is no content before the json starts other than the single
#'     optional code chunk
#'     \item the first non-json line following the json begins with a \code{#}
#' }
#' 
#' @param text chracter vector (1 line per element) of a Rmd or md config file
#' @return TRUE or FALSE indicating if the json is valid
#' @export
validate_config = function(text) {
    code_delims = which(str_detect(text, pattern = "^```"))
    if (1 %in% code_delims) {
        preamble = text[code_delims[1]:code_delims[2]]
        text = text[-(code_delims[1]:code_delims[2])]
    } else {
        preamble = character(0)
    }
    start_content = min(which(str_detect(text, pattern = "^#")))
    json = text[1:(start_content - 1)]
    content = text[-(1:(start_content - 1))]
    return(jsonlite::validate(json))
}