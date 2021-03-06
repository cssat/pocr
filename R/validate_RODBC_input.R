#' Validating requests for an RODBC connection.
#' 
#' @description
#' This is a helper function used to insure users provide valid input where
#' they need to specify a valid RODBC connection.
#' 
#' Here 'valid' means either an object of class RODBC or a character string 
#' that - when passed to \code{RODBC::odbcConnect()} - creates such an RODBC
#' object.
#' 
#' @return
#' The function returns a list with three elements. 
#' 
#' \code{x$test_result} will return boolean \code{TRUE} if an RODBC object is 
#' provided or was successfully created - it will be \code{FALSE} otherwise.
#' 
#' \code{x$test_message} returns a character string giving a more verbose
#' description of the success or fail. It can be inspected for details or
#' used to convey success/failure information to the user.
#' 
#' \code{x$connection} returns the created/validated connection. This is how 
#' the connection can be retrieved for further use.
#' 
#' @param connection An RODBC connection or character vector that can be
#' passed to odbcConnect to create an RODBC connection.
#' 
#' @export
validate_RODBC_input <- function(connection) {
    # check that RODBC connection or character vector provided
    if(missing(connection)) {
        test_result <- FALSE
        test_message <- "No input provided for the parameter. \n"
        connection <- NULL
    } else if(class(connection) != "RODBC" && 
              class(connection) != "character") {
        test_result <- FALSE
        test_message <- sprintf("'%s' must be an open RODBC connection or a ",
                                "character vector that can be used to create ",
                                "an RODBC connection. \n", connection)
    } else if(class(connection) == "RODBC") {
        test_result <- TRUE
        test_message <- sprintf("Success: %s is an open RODBC connection. \n",
                                deparse(substitute(connection)))
    } else if(class(connection) == "character") {
        try(test_connection <- odbcConnect(connection), silent = TRUE)
        
        if(class(test_connection) != "RODBC") {
            test_result <- FALSE
            test_message <- sprintf(paste0("Could not create an RODBC ",
                                           "connection from the provided ",
                                           "character vector, '%s'. \n"), 
                                    connection)
        } else {
            test_result <- TRUE
            test_message <- sprintf(paste0("Success: RODBC connection created ",
                                           "from character vector, '%s'. \n"), 
                                    connection)
            connection <- test_connection
        }
    }
    
    return(list("test_result" = test_result,
                "test_message" = test_message,
                "connection" = connection))
}