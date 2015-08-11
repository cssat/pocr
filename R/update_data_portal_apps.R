#' Update the data for Data Portal products with R generated data.
#' 
#' @description 
#' Most if not all Data Portal applications are dependent upon datasets 
#' tailored to each application's needs. 
#' 
#' It is the policy of the Data Portal team that data for such data-dependent 
#' applications must be generated either as part of Data Portal SQL loading or
#' by R processes defined in the \code{pocr} package.
#' 
#' This is the wrapper that updates the data for Data Portal products using
#' R generated data.
#' 
#' The user specifies which applications should receive a data update (the 
#' function defaults to updating all supported applications) and the wrapper
#' performs the necessary steps to:
#' \enumerate{
#'  \item Prepare data sources
#'  \item Gather updated data
#'  \item Process updated to application specifications
#'  \item Push processed data to application repos
#' }
#' 
#' The steps are performed application-by-application, with each application
#' having its own wrapper and sweet of functions to handle its specific needs.
#' 
#' On successful completion, the only remaining step will be pushing the
#' updated repos to production.
#' 
#' @param target_apps A string vector of application names to update. The names
#' match the pocdata repo names on GitHub. Defaults to "all" which updates
#' all supported applications.
#' @param excluded_apps A string vector of application names to avoid updating. 
#' Useful if you want to update most - but not all - applications. Defaults to 
#' NULL.
#' @param annie_connection An active RODBC connection to the "test_annie" MySQL
#' server or a character string that can be passed to 
#' \code{RODBC:odbcConnect()} to create an active RODBC connection to that 
#' server.
#' @param poc_connection An active RODBC connection to the "POC" SQL server
#' or a character string that can be passed to \code{RODBC:odbcConnect()} to 
#' create an active RODBC connection to that server.
#' @param review_mode A boolean that allows you to inspect the data for a
#' given update request. If TRUE, all data is returned in a structured list
#' for inspection and no data is pushed to GitHub. Defaults to FALSE.
#' 
#' @export
update_data_portal_apps <- function(target_apps = "all", 
                                    excluded_apps = NULL,
                                    annie_connection = "annie",
                                    poc_connection = "POC",
                                    review_mode = FALSE) {
    
}