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
#' Graduation/Dropout data link (found under Static Reports):
#' http://data.k12.wa.us:9990/PublicDWP/Web/WashingtonWeb/StaticDataFiles/1644_SDF_AppendixC_county_adjusted5yr.xlsx
#' 
#' Free/Reduced Lunch data link:
#' 
#' 
#' @param connection A valid RODBC connection to the SQL Server Database. It is
#' assumed that the connection has been checked with 
#' \code{validate_RODBC_input()} or has been otherwise validated - no
#' testing of the input will be done.
#' 
#' @export
update_ospi_data <- function(connection) {
    # define the data urls
    grad_url <- paste0("http://data.k12.wa.us:9990/PublicDWP/Web/",
                       "WashingtonWeb/StaticDataFiles/",
                       "1644_SDF_AppendixC_county_adjusted5yr.xlsx")
    # lunch_url <- paste0()
    
    # get the graduation/dropout dataframe
    grad_data <- get_ospi_grad_data(grad_url)
    
    # drop old OSPI tables
    
    # recreate updated OSPI tables
    
    # saving...
#     sql_server <- odbcConnect("POC")
#     sqlSave(sql_server, dat=grad_for_db, tablename="public_data.hs_graduation", rownames = FALSE)
    
    # temp - return data frames
    return(list("grad_data" = grad_data))
}