#' Get current data for Data Portal products with R generated data.
#' 
#' @description 
#' Most if not all Data Portal applications are dependent upon datasets 
#' tailored to each application's needs. 
#' 
#' It is the policy of the Data Portal team that data for such data-dependent 
#' applications must be generated either as part of Data Portal SQL loading or
#' by R processes defined in the \code{pocr} package.
#' 
#' This is the wrapper that generates current data for Data Portal products 
#' using R generated data. It is intended to be the first stage of the 
#' data update process for these products.
#' 
#' The user specifies which applications should receive a data update (the 
#' function defaults to updating all supported applications) and the wrapper
#' performs the necessary steps to:
#' \enumerate{
#'  \item Prepare data sources
#'  \item Gather updated data
#'  \item Process updated data to application specifications
#'  \item Write processed data to a local directory, named after the repo
#' }
#' 
#' The steps are performed application-by-application, with each application
#' having its own wrapper and suite of functions to handle its specific needs.
#' 
#' On successful completion, the user should manually inspect the generated
#' data. If it appears correct, the data should then be manually integrated
#' into each application's repo.
#' 
#' @param target_apps A string vector of application names to update. The names
#' match the pocdata repo names on GitHub. Defaults to "all" which updates
#' all supported applications.
#' @param excluded_apps A string vector of application names to avoid updating. 
#' Useful if you want to update most - but not all - applications. Defaults to 
#' NULL.
#' @param annie_connection An active RODBC connection to the "test_annie" MySQL
#' server or a character string that can be passed to 
#' \code{RODBC::odbcConnect()} to create an active RODBC connection to that 
#' server.
#' @param poc_connection An active RODBC connection to the "POC" SQL server
#' or a character string that can be passed to \code{RODBC::odbcConnect()} to 
#' create an active RODBC connection to that server.
#' 
#' @export
get_portal_app_data <- function(target_apps = "all", 
                                excluded_apps = NULL,
                                annie_connection = "annie",
                                poc_connection = "POC") {
    # define the list of applications, each with the repo name and a call
    # to the application data retrieval wrapper as a string that can be
    # evaluated
    apps_wraps <- list(
        "county_dashboard" = list(
            "call" = paste0("get_county_dashboard_data(",
                            "annie_connection, poc_connection)")
        ),
        
        "portal-browse" = list(
            "call" = "get_site_dashboard_data_wrapper(annie_connection)")
    )
    
    # assess which apps the user wants updated data for
    # first only retain those apps in "target_apps"
    if(target_apps != "all") {
        apps_wraps <- apps_wraps[names(apps_wraps) %in% target_apps]
    }
    
    # then drop any apps in excluded_apps
    if(!is.null(excluded_apps)) {
        apps_wraps <- apps_wraps[names(apps_wraps) %nin% excluded_apps]
    }
    
    # verify that at least one app is selected for data retrieval
    if(length(apps_wraps) < 1 | typeof(apps_wraps) != "list") {
        stop("No applications selected for data retrieval.")
    }
    
    # create/verify the MySQL connection (annie/test_annie)
    message("Validating the MySQL (annie) connection...")
    annie_test <- validate_RODBC_input(annie_connection)
    
    if(annie_test$test_result) {
        annie_connection <- annie_test$connection
        message(annie_test$test_message)
    } else {
        stop(annie_connection$test_message)
    }
    
    # create/verify that SQL Server connection (poc)
    message("Validating the SQL Server (poc) connection...")
    poc_test <- validate_RODBC_input(poc_connection)
    
    if(poc_test$test_result) {
        poc_connection <- poc_test$connection
        message(poc_test$test_message)
    } else {
        stop(poc_connection$test_message)
    }
    
    # create a app_data that we will populate with any retrieved data and
    # adjust the working directory to this folder
    folder_check <- safe_folder("app_data")
    if(folder_check$result) {
        setwd(folder_check$full_path)
    } else {
        stop("Unable to create the app_data folder: ", folder_check$message)
    }
    
    # call the selected retrieval functions, messaging the user so they know
    # what is happening
    for(app_index in 1:length(apps_wraps)) {
        current_app_name <- names(apps_wraps)[app_index]
        current_app_call <- apps_wraps[[app_index]]$call
        
        message(paste0("Getting data for ", 
                       current_app_name, 
                       "..."))
        
        result <- try(eval(parse(text = current_app_call)))
        
        message(paste0(result, "\n"))
    }
    
    # adjust the working directory back outside the created folder
    setwd("../")
    
    # pass a simple success message
    return("Data retrieval complete.")
}