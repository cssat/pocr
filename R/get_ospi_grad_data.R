#' Gather and clean OSPI graduation/dropout data.
#' 
#' @description 
#' Helper function that handles reading any supporting external data sources
#' and preparing the R dataframe that will be used to create the SQL table.
#' 
#' @param grad_url Need to provide a URL to the current graduation/dropout
#' data file. See \code{?update_ospi_data} for the general OSPI url and
#' directions.
#' 
#' @return 
#' Returns a cleaned R dataframe.
#' 
#' @export
get_ospi_grad_data <- function(grad_url) {
    # download the data from the url (necessary step since we're relying on
    # openxlsx - benefit of no problematic rJava, con of can't read from urls
    # directly)
    temp_name <- "temp_grad_data.xlsx"
    download.file(grad_url, temp_name, mode = "wb")
    
    # read the data from the temp file and try to clean-up the file (clean-up
    # may fail pending user permissions)
    grad_data <- read.xlsx(temp_name,
                           sheet = "All Students",
                           cols = c(1, 2, 8, 10),
                           rows = c(5:44))
    try(unlink(temp_name))
    
    # give the columns their preferred SQL server friendly names
    names(grad_data) <- c("county", "seniors", "grads", "grad_rate")
    
    # adjust grad_rate to be a proportion rather than a percentage
    grad_data$grad_rate <- grad_data$grad_rate / 100
    
    # create state summary data
    wa <- grad_data[1, ]
    wa$county <- "All"
    wa$seniors <- sum(grad_data$seniors)
    wa$grads <- sum(grad_data$grads)
    wa$grad_rate <- wa$grads / wa$seniors
    
    grad_data <- rbind(grad_data, wa)
    
    # join with the pocr ref_lookup_county dataframe to add county codes
    grad_data <- left_join(grad_data, 
                           ref_lookup_county[, c("county_cd", "county")])
    
    # specify the year for all rows
    grad_data$year <- 2011
    
    # return the prepared data
    return(grad_data)
}