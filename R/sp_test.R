#' @title
#' Tests \code{stored_procedure} functionality on the local machine.
#' 
#' @description
#' \code{stored_procedure} is the workhorse of \code{pocr}. However, the strings
#' it produces are only useful if the local machine is correctly configured to 
#' connect to the POC SQL server, the database(s) involved are up to date, and 
#' the stored procedures call by the strings are valid.
#' 
#' \code{sp_test} creates the SQL call strings for all available stored 
#' procedures and tests the results of attempting to use each one to retrieve
#' data from the POC SQL server.
#' 
#' This function allows you to quickly check if the strings produced by 
#' \code{stored_procedure} can be used to get valid results on the local 
#' machine.
#' 
#' The function will attempt to capture any errors and direct the user to their
#' likely cause(s).
#' 
#' This error detection is limited. The function will test if:
#' \itemize{
#'   \item The provided POC SQL server connection works for accessing the
#'   needed database(s) and table(s).
#'   \item Each of the available stored procedures return valid results.
#' }
#' 
#' @param connection An RODBC connection or a character vector that can be
#' passed to \code{odbcConnect} to create an RODBC connection appropriate for 
#' your local machine.
#' 
#' @export
sp_test <- function(connection) {
    # test if user provided valid input for the 'connection' parameter
    message("Testing if user provided valid input to 'connection' parameter...")
    
    if(missing(connection)) {
        stop("'connection' must be specified")
    }
    if(class(connection) != "RODBC" && class(connection) != "character") {
        stop(paste0("'connection' must be an open RODBC connection or a ",
                    "character vector naming a connection to use"))
    }
    message("Success. \n")
    
    # if a character vector has been provided, attempt to create the RODBC
    # connection object
    if(class(connection) == "character") {
        message("Creating RODBC connection from character vector...")
        
        try(test_connection <- odbcConnect(connection), silent = TRUE)
        
        # assess if any connection was made from the character vector - if
        # successful, replace 'connection' with the RODBC object
        if(class(test_connection) != "RODBC") {
            stop(sprintf(paste0("RODBC connectioned failed for the provided ",
                                "character vector, '%s'"), connection))
        } else {
            connection <- test_connection
            message("Success. \n")
        }
    }
    
    # attempt to get the current collection of stored procedures from the 
    # stored_procedure function definition
    message("Getting the current list of supported stored procedures...")
    
    try(sp_names <- eval(formals(pocr::stored_procedure)$sp), silent = TRUE)
    if(exists("sp_names") && !is.null(sp_names)) {
        message("Success. \n")
    } else {
        stop(paste0("unable to retrieve list from the function ", 
                    "pocr::stored_procedure"))
    }
}