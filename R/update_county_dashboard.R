#' Update the data for the County Dashboard application.
#' 
#' @description 
#' This is a helper function that pulls together the data needed by the
#' County Dashboard application (https://github.com/pocdata/county_dashboard),
#' formats the data as needed, and then pushes the formatted data to the
#' application GitHub repo.
#' 
#' Two collections of data are produced by \code{get_sparklines_data()} and
#' \code{get_fast_facts_data()}:
#' * data to support the trends over time (aka - the sparklines)
#' * data to support summary descriptive statistics (aka - the fast facts)
#' 
#' These collections are then split into county, region, and state variants, 
#' along with some crucial context and titling information for each variant,
#' by \code{format_dashboard_data()}.
#'
#' The final data results are:
#' * data_county.json   # sparklines/fast facts data
#' * titles_county.json # titling and sparklines/fast facts context
#' * data_region.json   
#' * titles_region.json 
#' * data_state.json
#' * titles_state.json
#' 
#' Data sources used in the above process:
#' * annie  variety of stored procedure calls
#' * POC    dbo.ref_lookup_county_region
#' * POC    dbo.ref_lookup_census_population
#' * POC    public_data.unemployment
#' * POC    public_data.hs_graduation
#' * POC    public_data.free_lunch_county
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
update_county_dashboard <- function(annie_connection, poc_connection) {
    # get the base data needed for the sparklines
    spark_base <- get_sparklines_data(annie_connection)
    
    # get the base data needed for the fast facts
    fact_base <- get_fast_facts(poc_connection)
    
    # shape the data into the dataframe structures needed by the application
    clean_data <- finish_dashboard_data(spark_base, fact_base)
    
    # check if there is a local folder for this project already
    folder_check <- file.exists("pocr")
    
    # if there is a folder, we create a unique temp folder to work with
    target_folder <- "temp"
    count_var <- 1
    while(file.exists(target_folder)) {
        if(count_var > 15) {
            stop("Update county dashboard cannot create a unique temp folder.")
        }
        target_folder <- paste0(target_folder, count_var)
    }
    dir.create(target_folder)
    
    # change the working directory to the temp folder
    setwd(target_folder)
    
    # clone the repo to the temp folder
    system("git clone https://github.com/pocdata/county_dashboard")
    system("bwaismeyer")
    system("78NEZZq3$7%S$f8")
    
    # remove the current data files
    list.files("county_dashboard/")
    
}