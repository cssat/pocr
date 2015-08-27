#' A simple test to see if R has write permissions.
#' 
#' @description 
#' This simply attempts to create a folder in the named directory (the 
#' working directory by default). If successful, the test folder is deleted.
#' 
#' To minimize the risk of overwriting or deleting an existing folder, the
#' test folder is created using \code{safe_folder()}. As a result, the test
#' may fail under the very unlikely circumstance that folders named "temp1"
#' to "temp15" exist in the test directory. The user will be notified should
#' this occur.
#' 
#' @param test_directory The directory you want to test against. Defaults to 
#' "." for the current working directory.
#' 
#' @return 
#' The function returns a boolean value \code{TRUE} on success (indicating
#' R has write permissions in the test directory). On failure, the function
#' returns a message articulating where the attempt to write failed.
#' 
#' @export
write_test <- function(test_directory = ".") {
    # we attempt to create a folder - "temp" based on the safe_folder()
    # defaults - in the test directory
    folder_test <- try(safe_folder(target_path = test_directory))
    
    # if the folder creation failed entirely (i.e., a non-valid object
    # was returned by safe_folder)...
    if(length(folder_test) != 3 | typeof(folder_test) != "list") {
        # set the output variable to indicate failure
        write_permissions <- paste0("The call to safe_folder() failed. ", 
                                    "Check that test_directory is valid. ",
                                    "If it is, you may need to experiment ",
                                    "with safe_folder() to find the cause.")
    # if the folder creation failed because we did not arrive at a valid
    # folder name or because write failed...
    } else if(!folder_test$result) {
        # set the output variable to pass on the details from safe_folder - 
        # this will id whether write failed or could not create a valid
        # folder name
        write_permissions <- folder_test$details
    # if we succeeded at making a folder...
    } else {
        # set the output variable to indicate success
        write_permissions <- TRUE
        # clean up the test folder
        unlink(folder_test$full_path, recursive = TRUE)
    }
    
    # return the test results
    return(write_permissions)
}