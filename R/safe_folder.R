#' Safely create a uniquely named folder.
#' 
#' @description 
#' When writing scripts that need to write to the local machine, we often
#' need to create a unique (often temporary) directory.
#' 
#' This function attempts to create a directory of the given name - or "temp"
#' if no name is given - but will avoid overwriting or recreating an already
#' existing directory.
#' 
#' If a directory of the given name exists, it will instead attempt to create
#' a variant of the given name. 
#' 
#' @param target_name The folder name the function will try to create.
#' @param target_path Where the folder should be created. Defaults to the 
#' current working directory (".").
#' 
#' @return 
#' The function will return a list with two items.
#' 
#' \code{x$result} will be a boolean. \code{TRUE} on success and \code{FALSE} on
#' failure.
#' 
#' \code{x$details} will be a character string explaining what occurred Probably
#' only interesting on failure.
#' 
#' \code{x$full_path} will be a character string giving the path of the created
#' folder. Useful for automation where you need to know what folder was
#' created.
#' 
#' On success, the given folder will also have been created in the target 
#' directory.
#' 
#' @export
safe_folder <- function(target_name = "temp", target_path = ".") {
    # if the target path doesn't end in a "/" we add it
    if(!grepl("/$", target_path)) {
        target_path <- paste0(target_path, "/")
    }
    
    # make the full path to test
    full_path <- paste0(target_path, target_name)
    
    # check if the target name exists in the target directory
    folder_check <- file.exists(full_path)
    
    # if there is a folder by that name, we try to create a variant on the name
    if(folder_check) {
        # define a counter that we can use both to append to the folder name
        # and to insure we don't get stuck in an infinite loop
        count_var <- 1
        
        test_path <- full_path
        
        # if the folder name exists...
        while(file.exists(test_path)) {
            # check if we have counted to a silly number and break out of the
            # loop if we have
            if(count_var > 15) {
                details <- sprintf(paste0("Unable to create a variant of the ",
                                          "target name - %s1 to ",
                                          "%s15 are unavailable."),
                                   full_path, full_path
                                   )
                break()
            }
            
            # otherwise, update the name with the current counter value,
            # increment the counter, and back to the top of the while loop
            test_path <- paste0(full_path, count_var)
            count_var <- count_var + 1
        }
        
        # whatever test path we finished with, adopt it as our target path
        full_path <- test_path
    }
    
    # if we failed to create a unique folder...
    if(file.exists(full_path)) {
        # set result to indicate our failure
        result <- FALSE
    # but if we succeeded...
    } else {
        # try to create the folder and verify the creation succeeded (may
        # fail if write permissions missing)
        try(dir.create(full_path))
        if(file.exists(full_path)) {
            result <- TRUE
            details <- paste0(full_path, " successfully created.")
        } else {
            result <- FALSE
            details <- paste0("You do not have write permissions for ",
                              target_path, ".")
        }
    }
    
    # collection and return the outputs
    return(list("result" = result,
                "details" = details,
                "full_path" = full_path))
}