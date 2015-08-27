#' Update the data for the County Dashboard application.
#' 
#' @description 
#' This is a helper function that pulls together the data needed by the
#' County Dashboard application (https://github.com/pocdata/county_dashboard),
#' formats the data as needed, and then writes the data to a folder sharing the
#' name of the project repo ("county_dashboard").
#' 
#' Two collections of data are produced by \code{get_sparklines_data()} and
#' \code{get_fast_facts_data()}:
#' \itemize{
#'  \item{data to support the trends over time (aka - the sparklines)}
#'  \item{data to support summary descriptive statistics (aka - the fast facts)}
#' }
#' 
#' These collections are then split into county, region, and state variants, 
#' along with some crucial context and titling information for each variant,
#' by \code{finish_dashboard_data()}.
#'
#' The final data results are:
#' \itemize{
#'  \item{county_context.csv (titling and sparklines/fast facts context)}
#'  \item{county_data.csv (sparklines/fast facts data)}
#'  \item{region_context.csv}
#'  \item{region_data.csv}
#'  \item{state_context.csv}
#'  \item{state_data.csv}
#'  \item{year_limits.csv (the min and max year covered by the data)}
#' }
#' 
#' Data sources used in the above process:
#' \itemize{
#' \item{annie: variety of stored procedure calls}
#' \item{POC: dbo.ref_lookup_county_region}
#' \item{POC: dbo.ref_lookup_census_population}
#' \item{POC: public_data.unemployment}
#' \item{POC: public_data.hs_graduation}
#' \item{POC: public_data.free_lunch_county}
#' }
#'
#' @param annie_connection Active RODBC connection the annie MySQL server. No
#' testing done to insure valid connection - should be handled external to this
#' function.
#' @param poc_connection Active RODBC connection the POC SQL server. No
#' testing done to insure valid connection - should be handled external to this
#' function.
#'
#' @return
#' Function attempts to verify if the update succeeds but failure is not
#' terminal to avoid interrupting the update collection.
#'
#' Returns a simple list. If all update steps seem succesful, first element
#' is TRUE and second element is character string "Success."
#'
#' If any update steps seem to fail, first element is FALSE and second element
#' is a character vector of detected issues.
#'
#' @export
get_county_dashboard_data <- function(annie_connection, poc_connection) {
    # get the base data needed for the sparklines
    spark_base <- get_sparklines_data(annie_connection)
    
    # get the base data needed for the fast facts
    fact_base <- get_fast_facts(poc_connection)
    
    # shape the data into the dataframe structures needed by the application
    clean_data <- finish_dashboard_data(spark_base, fact_base)
    
    # safely create a folder for the data - if called by update_data_portal_apps
    # there should be little chance of duplicate folder names (because that
    # creates a special data folder and adjust the working directory to that
    # folder), but if update_county_dashboard is called directly, we
    # minimize the risk of overwriting a folder with safe_folder
    folder_check <- safe_folder(target_name = "county_dashboard")
    
    # if creating the folder failed, we flag the update as a failure and 
    # end the function early
    if(!folder_check$result) {
        return(paste0("safe_folder() failed with the following: ",
                      folder_check$details))
    }
    
    # otherwise, we proceed to write a csv for each data object (vector or 
    # dataframe) to the target folder
    folder_name <- folder_check$full_path
    
    # for each item in the clean_data list...
    for(list_index in 1:length(clean_data)) {
        current_list <- clean_data[[list_index]]
        # make sure it is a list
        if(typeof(current_list) != "list") {
            # if not, it should just be a dataframe or vector and can be
            # written directly
            write.csv(current_list, 
                      paste0(folder_name, "/", 
                             names(clean_data[list_index]), ".csv"))
        } else {
            # but if it is a list, we are going to print each item
            # (presumably a dataframe) in that list
            for(df_index in 1:length(current_list)) {
                current_df <- current_list[[df_index]]
                df_name <- names(current_list[df_index])
                list_name <- names(clean_data)[[list_index]]
                write.csv(current_df, 
                          paste0(folder_name, "/",
                                 list_name, "_", df_name, ".csv"))
            }
        }
    }
    
    # and finally report success
    return("county_dashboard data successfully prepared.")
}