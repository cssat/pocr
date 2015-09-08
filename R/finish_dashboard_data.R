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
    # OUTPUT PART 1 OF 3: year_limits
    # extract the earliest and latest year from spark_base
    # to determine the start/end of the sparklines time axis;
    # the year range is the same for county, region, and state so we just
    # arbitrarily extract from the "state" dataframe
    year_limits <- range(spark_base$state$date)
    
    # OUTPUT PART 2 OF 3: context_collection
    # define the groups and measures that the context and content values will 
    # need to be distributed to; also define the value type we are expecting 
    # (how the values should be treated by the D3 app itself, f = float, 
    # p = percent, s = string)
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
    value_format <- c("f", "p", "p", "p", "p",
                      "s", "p", "p", "p", "p")
    
    # gather the names and titles into a dataframe to make them easy to 
    # work with and clean up the building blocks
    titles <- data.frame(group, code_name, pretty_name, value_format,
                         stringsAsFactors = FALSE)
    rm(group, code_name, pretty_name, value_format)

    # for the sparklines, we need to add the following to our base "titles"
    # object: global_min, global_max, current_min, current_max (global is 
    # across all years for which a measure has data, current is for the latest 
    # year for which a measure has data)
    
    # for the fast facts, we need just global_min and global_max (here, 
    # global is across all counties rather than all years)
    
    # we prepare a helper function to handle the key work of getting
    # mins and maxes for each measure
    get_min_max <- function(titles, spark_base, fact_base, geo_target) {
        # define the variables we want to fill with mins and maxes ('current'
        # only applies to sparklines measures - fast fact measures will
        # be assigned NA for both current_min and current_max)
        global_min <- c()
        global_max <- c()
        current_min <- c()
        current_max <- c()
        
        # for each of the passed code names...
        for(i in titles$code_name) {
            # check if it is a sparkline name - if yes, use the spark_base
            # data and flag to also get current_min/current_max
            if(grepl("trend", i)) {
                target_base <- spark_base[[geo_target]]
                get_current <- TRUE
            # otherwise it is a fast fact name - use the fact_base data
            # and don't try to get a current_min/current_max
            } else {
                target_base <- fact_base[[geo_target]]
                get_current <- FALSE
            }
            
            # get the global_min and global_max for the target column (the 
            # column with the same name as the current code_name)
            match_index <- which(names(target_base) == i)
            match_col <- target_base[, match_index]
            min_max <- range(match_col, na.rm = TRUE)
            
            # update our global_min and global_max collections with the values
            global_min <- c(global_min, min_max[1])
            global_max <- c(global_max, min_max[2])
            
            # if a sparklines measure (i.e., starts with "trend"), we also
            # want to get the current_min and current_max (i.e., the min and 
            # max for the most recent year with data)
            if(get_current) {
                # get the subset of the target dataframe which has non-NA values
                # for the target column
                data_index <- which(!is.na(target_base[, i]))
                target_base <- target_base[data_index, ]
                
                # get the latest year for the subset database (aka - the latest
                # year for which we have data in the target column)
                latest_year <- max(target_base$date)
                
                # subset again to get only data for the latest year with data
                latest_index <- which(target_base$date == latest_year)
                target_base <- target_base[latest_index, ]
                
                # get the range again for our target variable
                match_col <- target_base[, match_index]
                current_min_max <- range(match_col, na.rm = TRUE)
            # a fast fact measure, we just pass NA for the current_min and 
            # current_max
            } else {
                current_min_max <- c(NA, NA)
            }
            
            # update our current_min and current_max collections
            current_min <- c(current_min, current_min_max[1])
            current_max <- c(current_max, current_min_max[2])
        }
        
        # gather the results into a single dataframe and return it
        collection <- data.frame(global_min, global_max, 
                                 current_min, current_max)
        
        return(collection)
    }
    
    # we identify the geographic regions we'll need to loop over to get our
    # target data
    geo_targets <- c("county", "region", "state")
        
    # we loop over these with our helper function to get min/max values for each
    geo_values <- lapply(geo_targets, 
                         function(x) {
                             get_min_max(titles, spark_base, fact_base, x)
                         }
    )
    names(geo_values) <- geo_targets
    
    # finally, we join each of these to our "titles" object to produce the
    # geography-specific "title" objects we'll pass to the application
    context_collection <- lapply(geo_values, function(x) cbind(titles, x))
    
    # OUTPUT PART 3 OF 3: content_collection
    # for the sparklines, we need to collect the data values for each year
    # for each measure for each unique location (e.g., every county needs
    # it's own set of values for all sparkline measures)
    
    # for the fast facts, we need to collect the data values for each unique
    # location (the fact_base data does not have data over time)
    
    # in both cases, these values are already calculated and associated with
    # the unique locations - we just need to shape and merge their dataframes
    # to get them in the proper groupings
    
    # first we adjust both the sparklines and fast facts data to be "long"
    # format with our target columns
    # id -> location (specify place the values are associated with)
    # group -> new variable to specify whether this is sparkline or fast fact
    # foster_care_trend_id (0-4) or population_fast_fact_id (0-4) -> code_name
    # pretty_name -> new variable to associate pretty name with code name
    # time -> already exists for sparklines but needs to be added as NA to facts
    # value
    
    # sparklines adjustments (long; change location to id; add group column;
    # add code version of geo name; correct column order)
    spark_long <- lapply(spark_base,
                         function(x) gather(x, code_name, value, -id, -date))
    spark_long <- lapply(spark_long,
                         function(x) mutate(x, 
                                            pretty_location = id,
                                            group = "foster_care_trend"))
    spark_long <- lapply(spark_long,
                         function(x) {
                             x$code_name <- as.character(x$code_name)
                             return(x)
                         }
    )
    spark_long <- lapply(spark_long,
                         function(x) {
                             x$code_location <- gsub(" ", "_", 
                                                     x$pretty_location)
                             return(x)
                         }
    )
    spark_long <- lapply(spark_long,
                        function(x) x <- x[, c("code_location",
                                               "pretty_location", 
                                               "group", 
                                               "date", 
                                               "code_name",
                                               "value")])
    
    # fast facts adjustments (same as above but also add date column)
    fact_long <- lapply(fact_base,
                        function(x) gather(x, code_name, value, -id))
    fact_long <- lapply(fact_long,
                         function(x) {
                             x$code_name <- as.character(x$code_name)
                             return(x)
                         }
    )
    fact_long <- lapply(fact_long,
                        function(x) mutate(x,
                                           pretty_location = id,
                                           group = "population_fast_fact", 
                                           date = NA))
    fact_long <- lapply(fact_long,
                        function(x) {
                            x$code_location <- gsub(" ", "_", 
                                                    x$pretty_location)
                            return(x)
                        }
    )
    fact_long <- lapply(fact_long,
                         function(x) x <- x[, c("code_location",
                                                "pretty_location", 
                                                "group", 
                                                "date", 
                                                "code_name",
                                                "value")])
    
    # now we put spark_long and fact_long together
    merged_long <- list("county" = rbind(spark_long$county,
                                         fact_long$county),
                        "region" = rbind(spark_long$region,
                                         fact_long$region),
                        "state" = rbind(spark_long$state,
                                        fact_long$state))
    
    # finally, we prepare the data object to pass out of the function, 
    # reorganizing our collections by region
    county <- list("context" = context_collection$county,
                   "data" = merged_long$county)
    region <- list("context" = context_collection$region,
                   "data" = merged_long$region)
    state <- list("context" = context_collection$state,
                  "data" = merged_long$state)
    
    # and then gathering these with the year_limits
    final_collection <- list("year_limits" = year_limits,
                             "county" = county,
                             "region" = region,
                             "state" = state)
    
    return(final_collection)    
}