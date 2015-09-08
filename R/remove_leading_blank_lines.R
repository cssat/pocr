#' Removes blank lines from the top of a file
#' 
#' Reads in text (usually from a file), removes any blank lines (or lines containing
#' only whitespace) from the top of the file, (optionally) writes the modified
#' file, invisibly returns the modified text.
#' 
#' @param input character path to the file
#' @param output character path to output file (defaults to overwrite input). Set to \code{NA}
#' to disable output.
#' @return invisibly returns the modified text
#' 
#' @export
remove_leading_blank_lines = function(input, output = input) {
    file = readLines(input)

    # Detect runs of lines with nothing but whitespace
    blank_line_rle = rle(grepl(pattern = "^\\s*$", x = file))
    
    # do nothing if first line is not blank
    if (!blank_line_rle$values[1]) return()
    
    # remove blank lines from front
    file = file[-(1:blank_line_rle$lengths[1])]
    
    # save if appropriate
    if (!is.na(output)) writeLines(text = file, con = output)
    
    invisible(text)
}
