#' Vectorized version of \code{county_to_office}.
#'
#' Takes a vector of counties (string or numeric) and gives a list of
#' the corresponding rows of ref_lookup_office
#' 
#' @param county string or numeric county_cd
#' @export
county_to_office_v <- Vectorize(county_to_office, SIMPLIFY=FALSE)