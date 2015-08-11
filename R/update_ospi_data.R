#' Update OSPI data.
#' 
#' @description
#' This function updates the POC SQL Server database with the most current
#' versions of data extracted from the OSPI website. This includes data
#' pertaining to high school graduation rates (public_data.hs_graduation)
#' and free and reduced lunch (public_data.free_lunch_county).
#' 
#' The function simply reads the relevant Excel files from the host website,
#' formats the data, and then uses the data to drop and re-create the relevant
#' tables in the POC SQL server database.
#' 
#' @param connection A valid RODBC connection to the SQL Server Database. It is
#' assumed that the connection has been checked with 
#' \code{validate_RODBC_input()} or has been otherwise validated - minimal
#' testing of the input will be done.
#' 
#' @export
update_ospi_data <- function() {
    
}