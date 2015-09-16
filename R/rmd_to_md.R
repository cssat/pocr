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
#' @param validate boolean indicating whether or not to
#' validate the json.
#' 
#' @return No value returned.
#'
#' @import knitr stringr
#' @export
rmd_to_md = function(base_path = getwd(), validate = TRUE) {
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
    
    # validate RMarkdown files 
    if (validate) {
        rmd_content = lapply(paste0(rmd_path, rmd_files), readLines)
        valid = sapply(rmd_content, validate_config)
        if (all(valid)) {
            message("All Rmd json valid.")
        } else {
            invalids = rmd_files[!valid]
            stop("The following Rmd files have invalid json:\n", paste("    ", invalids, collapse = "\n"))
        }
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
    
    # validate markdown files 
    if (validate) {
        md_content = lapply(paste(base_path, md_files, sep = "/"), readLines)
        valid = sapply(md_content, validate_config)
        if (all(valid)) {
            message("All md json valid.")
        } else {
            invalids = md_files[!valid]
            stop("The following md files have invalid json:\n", paste(invalids, collapse = "\n"))
        }
    }
    
    invisible()
}
