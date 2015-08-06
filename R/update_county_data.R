#' Function for updating county data from SQL Server
update_county_data <- function() {
    con <- odbcConnect("test_annie")
    ref_lookup_county <- sqlQuery(con, 
                                  "SELECT * FROM ref_lookup_county", 
                                  stringsAsFactors = FALSE)
    ref_lookup_county <- ref_lookup_county[c(2:40, 1), ]
    save(ref_lookup_county, 
         file = "G:/CA data transfer files/R/Gregor/poc/data/ref_lookup_county.rda")
    
    ref_lookup_office <- sqlQuery(con, 
                                  "SELECT * FROM ref_lookup_office_collapse;", 
                                  stringsAsFactors = FALSE)
    names(ref_lookup_office)[1:2] <- c("cd_office", "tx_office")
    save(ref_lookup_office, 
         file = "G:/CA data transfer files/R/Gregor/poc/data/ref_lookup_office.rda")
}