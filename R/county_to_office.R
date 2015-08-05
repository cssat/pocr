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