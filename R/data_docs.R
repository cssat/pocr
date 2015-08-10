###############################################################################
## data_docs.R

# This is an alphabetized collection of roxygen code to create documentation 
# for pocr data objects. 

# All built-in pocr data objects should be defined and described here.

###############################################################################

#' @title poc_colors
#' @description Vector of 4 hex colors from POC theme.
#' @docType data
#' @name poc_colors
NULL

#' @title portal_colors
#' @description Vector of 12 hex colors used in the portal
#' @docType data
#' @name portal_colors
NULL

#' @title Key to \code{quickfacts}
#' @description Verbose descriptions of \code{quickfacts} columns.
#' @docType data
#' @name qf_dict
NULL

#' @title quickfacts (from US Census Bureau)
#' @description Data frame characteristics of each county and of the state. 
#' \code{qf_dict} has the column keys.
#' @docType data
#' @name quickfacts
NULL

#' @title ref_lookup_county (from test_annie)
#' @description Data frame with columns for the county_cd, the text county
#' and the regional code cd_region. Taken from SQL Server table of the same 
#' name. I've ordered the entries so that row number corresponds to county_cd 
#' for the actual counties 1:39.
#' @docType data
#' @name ref_lookup_county
NULL

#' @title ref_lookup_county_region (from test_annie)
#' @description Data frame with columns for the county_cd, the text county
#' and the regional code cd_region. Taken from SQL Server table of the same 
#' name. I've ordered the entries so that row number corresponds to county_cd 
#' for the actual counties 1:39. Additionally, has columns for region, the
#' most useful of which are probably region_6_cd, region_6_tx, and small_fl.
#' @docType data
#' @name ref_lookup_county_region
NULL

#' @title ref_lookup_office (from test_annie)
#' @description Data frame with columns cd_office, tx_office, 
#' cd_office_county_grp, and tx_office_county_grp. Taken from SQL Server table 
#' of the same name.
#' @docType data
#' @name ref_lookup_office
NULL