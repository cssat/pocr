#' Takes a county (string or numeric) and gives
#' the corresponding rows of ref_lookup_office
#' 
#' @param county string or numeric county_cd
#' @export
county_to_office <- function(county) {
    if (is.numeric(county)) county <- ref_lookup_county$county[ref_lookup_county$county_cd == county]
    county <- tolower(county)
    if (county == "clark") county <- "clark "
    ref_lookup_office[which(!is.na(str_match(tolower(ref_lookup_office[, 4]),
                                             pattern = fixed(county)))), ]
}



#' Vectorized version of \code{county_to_office}.
#'
#' Takes a vector of counties (string or numeric) and gives a list of
#' the corresponding rows of ref_lookup_office
#' 
#' @param county string or numeric county_cd
#' @export
county_to_office_v <- Vectorize(county_to_office, SIMPLIFY=FALSE)



#' Function for updating county data from SQL Server
update_county_data <- function() {
    con <- odbcConnect("test_annie")
    ref_lookup_county <- sqlQuery(con, "SELECT * FROM ref_lookup_county", stringsAsFactors = FALSE)
    ref_lookup_county <- ref_lookup_county[c(2:40, 1), ]
    save(ref_lookup_county, file = "G:/CA data transfer files/R/Gregor/poc/data/ref_lookup_county.rda")
    
    ref_lookup_office <- sqlQuery(con, "SELECT * FROM ref_lookup_office_collapse;", stringsAsFactors = FALSE)
    names(ref_lookup_office)[1:2] <- c("cd_office", "tx_office")
    save(ref_lookup_office, file = "G:/CA data transfer files/R/Gregor/poc/data/ref_lookup_office.rda")
}


#' ref_lookup_county (from test_annie)
#' 
#' Data frame with columns for the county_cd, the text county
#' and the regional code cd_region. Taken from SQL Server table of the same name.
#' I've ordered the entries so that row number corresponds to county_cd for the
#' actual counties 1:39.
#' 
#' @format A data frame with three columns, county_cd, county, and region_cd
#' @docType data
#' @name ref_lookup_county



#' @title ref_lookup_office (from test_annie)
#' @description Data frame with columns cd_office, tx_office, cd_office_county_grp,
#' and tx_office_county_grp. Taken from SQL Server table of the same name.
#' @docType data
#' @name ref_lookup_office



#' @title quickfacts (from US Census Bureau)
#' @description Data frame characteristics of each county and of the state. \code{qf_dict} has
#' the column keys.
#' @docType data
#' @name quickfacts



#' @name qf_dict
#' @title Key to \code{quickfacts}
#' @description Verbose descriptions of \code{quickfacts} columns.
#' @docType data



#' Stored Procedure Names
#' @description sp_names returns the nams of all stored procedures.
#' @docType data
