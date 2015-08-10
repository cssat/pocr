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
#' your local machine and the server you want to test.
#' @param target_server POC has two different repositories you may want to 
#' test against, test-annie (uses MySQL) and poc (uses SQL server). You need
#' to specify which to test against: 'test_annie' or 'mysql' (equivalent), 
#' 'poc' or 'sqlserver' (equivalent).
#' 
#' @return
#' When run, the function sends messages to the console describing the phases
#' the function is running through. If the function completes without any
#' connection or package errors, it returns a list with three pieces. 
#' 
#' The first piece is a data frame ($sp_summary) summarizing the fate of 
#' each stored procedure call to the target server. This provides a quick
#' summary of whether each stored procedure returned a data frame and states
#' the number of columns and rows in the return.
#' 
#' The second is a data frame ($sp_strings) of the stored procedure names and 
#' the strings prepared for each by the stored_procedure function. This is 
#' useful for checking that valid strings were prepared and for seeing exactly 
#' what was passed to the server.
#' 
#' The third is a named list ($sp_details) of the data returned by each 
#' stored procedure call. This is useful for investigating stored procedures 
#' that were problematic and for manual inspection of stored procedure results 
#' in general.
#' 
#' @export
sp_test <- function(connection, target_server = "test_annie") {
    # test if user provided valid input for the 'connection' parameter
    message("Testing if 'connection' input valid...")
    
    if(missing(connection)) {
        stop("'connection' must be specified")
    }
    if(class(connection) != "RODBC" && class(connection) != "character") {
        stop(paste0("'connection' must be an open RODBC connection or a ",
                    "character vector naming a connection to use"))
    }
    message("Success. \n")
    
    # test if user provided valid input for the 'target_server" parameter
    message("Testing if 'target_server' input valid...")
    if(target_server %in% eval(formals(pocr::stored_procedure)$db)) {
        message("Success. \n")
    } else {
        stop("'target_server' must name a valid POC server - see ?sp_test")
    }
    
    # if a character vector has been provided, attempt to create the RODBC
    # connection object
    if(class(connection) == "character") {
        message(sprintf(paste0("Creating RODBC connection from character ",
                               "vector '%s'..."), connection))
        
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
    
    # create a call for each stored procedure with the default arguments (all
    # 0s)
    message("Creating a vector with all basic stored procedure strings...")
    
    sp_strings <- unlist(lapply(sp_names, 
                                function(x) pocr::stored_procedure(x, 
                                                                   target_server)))
    message("Success. \n")
    
    # run each stored procedure call with the given RODBC connection and collect
    # the results
    message("Attempting to run each stored procedure against target server...")
    sp_details <- lapply(sp_strings, 
                         function(x) sqlQuery(connection, x))
    message("Success. \n")
    
    # if the connection was created by the function (because the user passed
    # a character string), we close the connection for tidy practice
    if(exists("test_connection")) {
        message("Closing function-created RODBC connection...")
        close(connection)
        message("Success. \n")
    }
    
    # assess the call details to assess if the returned items are data frames
    # and try to count their rows and columns
    message("Checking and summarizing stored procedure results...")
    sp_returns_df <- lapply(sp_details, is.data.frame)
    num_col <- lapply(sp_details, ncol)
    num_row <- lapply(sp_details, nrow)
    sp_summary <- data.frame(cbind(sp_names,
                                   sp_returns_df,
                                   num_col,
                                   num_row))
    message("Success. \n")
    
    # format the strings and details for easy navigation between the
    # various test objects
    sp_strings <- data.frame(cbind(sp_names,
                                   sp_strings))
    names(sp_details) <- sp_names
    
    # gather the key test materials
    message("Gathering test results...")
    test_results <- list("sp_summary" = sp_summary,
                         "sp_strings" = sp_strings, 
                         "sp_details" = sp_details)
    message("Success. \n")
    
    # assign the results the sp_results class so they print nicely
    class(test_results) <- "sp_results"
    
    return(test_results)
}

#' Print method for sp_test.
#' 
#' Simple method to insure that the result turned by sp_test only prints out
#' the concise summary by default. Not exported.
#' 
#' @param x Object to print (inherited from print).
#' @param ... Additional arguments to pass to print.
print.sp_results = function(x, ...) {
    print(x$sp_summary)
}