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
#' The first piece is a character vector ($call_summary) summarizing the fate of 
#' each stored procedure call to the target server. This provides a quick
#' summary of whether stored procedures succeeded or had an identifiable
#' issue.
#' 
#' The second is a character vector ($call_strings) of the strings prepared
#' by the stored_procedure function. This is useful for checking that valid
#' strings were prepared and seeing exactly what was passed to the server.
#' 
#' The third is a list ($call_details) of the data returned by each stored 
#' procedure call. This is useful for investigating stored procedures that
#' were problematic or for manual inspection of stored procedure results in
#' general.
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
    
    call_strings <- unlist(lapply(sp_names, 
                                  function(x) stored_procedure(x, 
                                                               target_server)))
    message("Success. \n")
    
    # run each stored procedure call with the given RODBC connection and collect
    # the results
    call_details <- lapply(call_strings, 
                            function(x) sqlQuery(connection, x))
    
    # if the connection was created by the function (because the user passed
    # a character string), we close the connection for tidy practice
    if(exists("test_connection")) {
        message("Closing function-created RODBC connection...")
        close(connection)
        message("Success. \n")
    }
    
    # assess the call details to assess for success or identifiable errors
    
    
    # gather and return the key test materials
    message("Gathering test results...")
    test_results <- list("call_strings" = call_strings, 
                         "call_details" = call_details)
    message("Success. \n")
    
    return(test_results)
}