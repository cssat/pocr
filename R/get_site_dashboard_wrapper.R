#' Wrapper for .
#' 
#' @description This is a function to update the data for the website dashboards that 
#' should be run after each data load.
#' 
#' @param annie_connection A character string for ODBC connection defaulted to \code{"annie"}
#' 
#' @return The function returns a json object which is placed in a new folder when 
#' \code(get_portal_app_data()) is run. 
#' 
#' @export

get_site_dashboard_data_wrapper <- function(annie_connection){
    
    dashboard_json <- get_site_dashboard_data(con = annie_connection)

    folder_check <- safe_folder(target_name = "site_dashboard")
     
    # if creating the folder failed, we flag the update as a failure and 
    # end the function early
    
    if(!folder_check$result) {
        return(paste0("safe_folder() failed with the following: ", folder_check$details))
    }
    
    # otherwise, we proceed to write a csv for each data object (vector or 
    # dataframe) to the target folder
    
    folder_name <- folder_check$full_path
    
    write(dashboard_json, paste0(folder_name, '/', 'site_dashboard.json'))
}
