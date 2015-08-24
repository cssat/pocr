#' Helper function to finalize the data for the County Dashboard application.
#' 
#' @description 
#' This function takes the results from \code{get_sparklines_data()} and
#' \code{get_fast_facts_data()} and converts the data to the structure and 
#' format the County Dashboard application expects.
#' 
#' The final data structure is divided as follows:
#' - metadata specifying the "window" of time covered by the data (year start
#'   to year end) - impacts the context for the collection of sparklines in that
#'   it sets the time axis limits
#' - county data
#'  - "context": the values used to create the visualization context (e.g., 
#'    names, min/max values)
#'  - "data": the values used to create the visualization contents (e.g., the
#'    trend lines)
#' - region data
#'  - "context"
#'  - "data"
#' - state data
#'  - "context"
#'  - "data"
#' 
#' @param spark_base The list object with county, region, and state data
#' needed by the sparklines.
#' @param fact_base The list object with county, region, and state data needed
#' by the fast facts.
#' 
#' @return 
#' The function returns a list with the data divided into list elements.
#' * \code{x$year_limits} - vector with the start year and end year extracted
#'                          from spark_base
#' * \code{x$county} - list with a dataframe for "context" and one for "data"
#' * \code{x$region} - list with a dataframe for "context" and one for "data"
#' * \code{x$state}  - list with a dataframe for "context" and one for "data"
#' 
#' @export
finish_dashboard_data <- function(spark_base, fact_base) {
    # year_limits: extract the earliest and latest year from spark_base
    # to determine the start/end of the sparklines time axis;
    # the year range is the same for county, region, and state so we just
    # arbitrarily extract from the "state" dataframe
    year_limits <- range(spark_base$state$time)
    
    # define the names and titles that the context and content values will need
    # to be distributed to
    group <- c(rep("foster_care_trend", 5), 
               rep("population_fast_fact", 5))
    code_name <- c(paste0("trend_", 0:4), 
                   paste0("population_", 0:4))
    pretty_name <- c("Entries per 1,000",
                     "Entries Age 0-4",
                     "Entries with Immediate Dependency",
                     "Placements in Kin Care",
                     "Siblings All or Some Together",
                     "Population < 18",
                     "% Children < 5",
                     "Unemployment",
                     "H.S. Grad Rate",
                     "Free/Reduced Lunch")
    
    # gather the names and titles into a dataframe to make them easy to 
    # work with and clean up the building blocks
    titles <- data.frame(group, code_name, pretty_name, 
                         stringsAsFactors = FALSE)
    rm(group, code_name, pretty_name)

    
}