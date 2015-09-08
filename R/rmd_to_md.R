#' Creates md config files from Rmd source
#' 
#' Works well if your working directory is the root
#' of a portal content repo. (You can alse give a filepath).
#' Looks in a subdirectory called \code{Rmarkdown/} for 
#' \code{.Rmd}
#' files, knits them to \code{.md} and removes any
#' blank lines from the top.
#' 
#' @param base_path character path to directory.
#' Defaults to the working directory.
#' Must have a subdirectory called \code{RMarkdown}).
#' 
#' @return No value returned.
#'
#' @import knitr stringr
#' @export
rmd_to_md = function(base_path = getwd()) {
    rmd_path = paste(base_path, "RMarkdown/", sep = "/")
    # Get RMarkdown files to knit
    rmd_files <- list.files(pattern = '*\\.Rmd$',
                            path = rmd_path,
                            ignore.case = TRUE)
    # Make sure we found at least 1 file
    if (length(rmd_files) == 0) {
        warning("No RMarkdown files found.")
        invisible()
    }
    
    # Create markdown names
    md_files = str_replace(rmd_files, "Rmd$", "md")
    
    # knit the files to markdown
    for (i in 1:length(rmd_files)){
        knit(input = paste(rmd_path, rmd_files[i], sep = "/"),
             output = paste(base_path, md_files[i], sep = "/"))
    }
    
    # Remove blank lines from the top
    sapply(md_files, remove_leading_blank_lines)
    invisible()
}
