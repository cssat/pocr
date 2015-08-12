#' Gather and clean OSPI free/reduced lunch data.
#' 
#' @description 
#' Helper function that handles reading any supporting external data sources
#' and preparing the R dataframe that will be used to create the SQL table.
#' 
#' @param lunch_url Need to provide a URL to the current graduation/dropout
#' data file. See \code{?update_ospi_data} for the general OSPI url and
#' directions.
#' 
#' @return 
#' Returns a cleaned R dataframe.
#' 
#' @export
get_ospi_lunch_data <- function(lunch_url) {
    # download the data from the url (necessary step since we're relying on
    # openxlsx - benefit of no problematic rJava, con of can't read from urls
    # directly)
    temp_name <- "temp_lunch_data.xlsx"
    download.file(lunch_url, temp_name, mode = "wb")
    
    # read the data from the temp file and try to clean-up the file (clean-up
    # may fail pending user permissions)
    lunch_data <- read.xlsx(temp_name)
    try(unlink(temp_name))
    
    # return the prepared data
    return(lunch_data)
}