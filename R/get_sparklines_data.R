#' Helper function to get and clean County Dashboard "sparklines" data.
#' 
#' @description 
#' This function gathers the base data needed for the County Dashboard trend 
#' plots (aka - the sparklines) and handles initial cleaning and preparation
#' of this data. 
#' 
#' Once finished, the sparklines data - along with the fast facts data
#' gathered by \code{get_fast_facts_data()} - is handed off to 
#' \code{finalize_dashboard_data()} to put it in the final format needed
#' for app consumption.
#' 
#' @param annie_connection Active RODBC connection to one of the annie MySQL
#' servers.
#' @param year_start Optional. Can specify the start year as a character
#' vector. If NA, the data sources will be assessed for a smallest minimum.
#' @param year_end Optional. Can specify the end year as a character vector.
#' If NA, the data sources will be assessed for a largest maximum.
#' 
#' @return
#' Returns a list with three data frames - county, region, and state - needed
#' to support the different trend line views. These data frames need further
#' processing prior to app consumption.
#' 
#' @export
get_sparklines_data <- function(annie_connection, 
                                year_start = NA, 
                                year_end = NA) {
    # first we need to get a variety of data from the annie MySQL server -
    # this is accomplished using pocr::stored_procedure()
    
    # a simple wrapper function to handle the steps common to all of the
    # queries - ... here allows for additional arguments to be passed to
    # stored_procedure()
    query_wrapper <- function(sp_name, con = annie_connection, ...) {
        # basic query by county
        target <- sqlQuery(con,
                           stored_procedure(sp = sp_name,
                                            db = "mysql",
                                            county = c(0:39),
                                            ...),
                           stringsAsFactors = FALSE
        )
        # correct the variable names
        names(target) <- tolower(make.names(names(target)))
        
        # return the target for filtering
        return(target)
    }
    
    # fetch and clean the basic data that will need to be combined
    # to make the county, region, and state data;
    # data is from the first day of each year (e.g., 2000-01-01)
    entry_counts <- query_wrapper(sp_name = "ooh_flow_entries_counts")
    entry_counts <- entry_counts %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        mutate(year = year(month)) %>%
        select(entry.cnt = number.of.entries, year, county_cd)
    
    entry_rates <- query_wrapper(sp_name = "ooh_flow_entries_rates")
    entry_rates <- entry_rates %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        mutate(year = year(month)) %>%
        select(entry.rate = rate.of.entries, year, county_cd)
    
    entry_0_to_4 <- query_wrapper(sp_name = "ooh_flow_entries_counts",
                                  age = 1)
    entry_0_to_4 <- entry_0_to_4 %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        mutate(year = year(month)) %>%
        select(entry.cnt.04 = number.of.entries, year, county_cd)
    
    entry_depend <- query_wrapper(sp_name = "ooh_flow_entries_counts",
                                  dependency = 2)
    entry_depend <- entry_depend %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        mutate(year = year(month)) %>%
        select(entry.cnt.dep = number.of.entries, year, county_cd)
    
    placement_kin <- query_wrapper("ooh_wb_familysettings")
    placement_kin <- placement_kin %>% 
        filter(qry_type_poc1_first_all == 2) %>%
        mutate(month = as.Date(date)) %>%
        mutate(year = year(month)) %>%
        select(pit.kin = family.setting..kin.placement., year, county_cd)
    
    placement_counts <- query_wrapper("ooh_pit_counts")
    placement_counts <- placement_counts %>% 
        filter(date_type == 1 & qry_type_first_all == 2) %>%
        mutate(date = as.Date(date)) %>%
        mutate(month = month(date)) %>%
        mutate(year = year(date)) %>%
        filter(month == 1) %>%
        select(pit.count = total.in.out.of.home.care.1st.day, year, county_cd)
    
    placement_sibling <- query_wrapper("ooh_wb_siblings_uncensored")
    placement_sibling <- placement_sibling %>% 
        filter(qry_type_poc1_first_all == 2 & 
                   bin_sibling_group_size == 0 & 
                   placement.type == "All Out-of-Home Care") %>%
        mutate(any.together = some.together + all.together) %>%
        select(pit.sib.pct = any.together,
               n.sibs = number.in.cohort,
               year,
               county_cd)
    
    # determine the time span the aggregated data will cover - if not explicitly
    # specified, this simply detects the largest min and smallest max from
    # across the data sources
    
    # define the collection of data objects (supports detecting year mins/maxes)
    data_collection <- list(entry_counts,
                            entry_rates,
                            entry_0_to_4,
                            entry_depend,
                            placement_kin,
                            placement_counts,
                            placement_sibling)
    
    # check if year_start defined
    if(is.na(year_start)) {
        # get the minimum year observed across all the data, use it as the start
        year_mins <- sapply(data_collection, function(x) min(x$year))
        year_start <- min(year_mins)
    }
    
    # check if year end defined
    if(is.na(year_end)) {
        # get the maximum year observed across all the data, use it as the end
        year_maxes <- sapply(data_collection, function(x) max(x$year))
        year_end <- max(year_maxes)
    }
    
    # build the base object with respect to the defined time range
    trends_base <- expand.grid(county_cd = 0:39,
                               year = seq(year_start, year_end))
    
    # join the collected data to this object (plus ref_lookup_county_region,
    # built-in pocr data)
    trends <- trends_base %>%
        left_join(entry_counts) %>%
        left_join(entry_rates) %>%
        left_join(entry_0_to_4) %>%
        left_join(entry_depend) %>%
        left_join(placement_kin) %>%
        left_join(placement_counts) %>%
        left_join(placement_sibling) %>%
        left_join(ref_lookup_county_region) %>% 
        select(-old_region_cd) %>%
        mutate(entry.pct.04 = entry.cnt.04 / entry.cnt,
               entry.pct.dep = entry.cnt.dep / entry.cnt,
               pit.sib.pct = pit.sib.pct / 100,
               pit.kin = pit.kin / 100)
    
    trends$agg.ent.dep[trends$year < 2003] <- NA
    
    # clean the data for counties
    trends_county <- trends %>% 
        filter(county_cd > 0) %>%
        mutate(county_small_cd = ifelse(small_fl == 1, -1, county_cd),
               county_small_tx = ifelse(small_fl == 1, "small", county)) %>%
        group_by(county_small_cd, year) %>%
        mutate(agg.ent.rate = weighted.mean(x = mean(entry.rate, na.rm = T),
                                            w = sum(entry.cnt, na.rm = T), 
                                            na.rm = T),
               agg.ent.04   = pmin(sum(entry.cnt.04, na.rm = T) / 
                                       sum(entry.cnt, na.rm = T), 1),
               agg.ent.dep  = pmin(sum(entry.cnt.dep, na.rm = T) / 
                                       sum(entry.cnt, na.rm = T), 1),
               agg.kin      = pmin(weighted.mean(mean(pit.kin, na.rm = T), 
                                                 w = sum(pit.count, na.rm = T), 
                                                 na.rm = T), 1),
               agg.sibs     = pmin(weighted.mean(pit.sib.pct, 
                                                 w = n.sibs, 
                                                 na.rm = T), 1)) %>%
        ungroup() %>%
        filter(!is.na(year)) %>%
        mutate(agg.ent.dep = ifelse(year < 2003, NA, agg.ent.dep)) %>%
        select(county, year, 
               agg.ent.rate, agg.ent.04, agg.ent.dep, agg.kin, agg.sibs)
    
    names(trends_county) <- c("id", "time", paste0("trend_", 0:4))
    
    # clean the data for regions
    trends_region <- trends %>% 
        mutate(region_6_tx = as.character(region_6_tx)) %>%
        filter(county_cd > 0) %>%
        group_by(region_6_tx, year) %>%
        summarize(agg.ent.rate = weighted.mean(x = entry.rate, 
                                               w = entry.cnt, 
                                               na.rm = T),
                  agg.ent.04   = pmin(sum(entry.cnt.04, na.rm = T) / 
                                          sum(entry.cnt, na.rm = T), 1),
                  agg.ent.dep  = pmin(sum(entry.cnt.dep, na.rm = T) / 
                                          sum(entry.cnt, na.rm = T), 1),
                  agg.kin      = pmin(weighted.mean(pit.kin, 
                                                    w = pit.count, 
                                                    na.rm = T), 1),
                  agg.sibs     = pmin(weighted.mean(pit.sib.pct, 
                                                    w = n.sibs, 
                                                    na.rm = T), 1)) %>%
        select(region_6_tx, year,
               agg.ent.rate, agg.ent.04, agg.ent.dep, agg.kin, agg.sibs) %>%
        mutate(agg.ent.dep = ifelse(year < 2003, 
                                    NA, 
                                    agg.ent.dep))
    
    names(trends_region) <- c("id", "time", paste0("trend_", 0:4))
    
    # clean the data for state
    trends_state <- trends %>% filter(county_cd == 0) %>%
        group_by(year) %>%
        summarize(agg.ent.rate = weighted.mean(x = entry.rate, 
                                               w = entry.cnt, 
                                               na.rm = T),
                  agg.ent.04   = pmin(sum(entry.cnt.04, na.rm = T) / 
                                          sum(entry.cnt, na.rm = T), 1),
                  agg.ent.dep  = pmin(sum(entry.cnt.dep, na.rm = T) / 
                                          sum(entry.cnt, na.rm = T), 1),
                  agg.kin      = pmin(weighted.mean(pit.kin, 
                                                    w = pit.count, 
                                                    na.rm = T), 1),
                  agg.sibs     = pmin(weighted.mean(pit.sib.pct, 
                                                    w = n.sibs, 
                                                    na.rm = T), 1)) %>%
        mutate(region_6_tx = "Washington") %>%
        select(region_6_tx, year,
               agg.ent.rate, agg.ent.04, agg.ent.dep, agg.kin, agg.sibs) %>%
        mutate(agg.ent.dep = ifelse(year < 2003, 
                                    NA, 
                                    agg.ent.dep))
    
    names(trends_state) <- c("id", "time", paste0("trend_", 0:4))
    
    # collect the county, region, and state data and make sure any non-values
    # (e.g., NA, NaN, Inf, etc.) are treated as NA
    cleaned_data <- list("county" = trends_county,
                         "region" = trends_region,
                         "state" = trends_state)
    
    # quick helper function to clean a single df
    replace_inf_nan <- function(df) {
        # for each column...
        for(i in 1:ncol(df)) {
            # ID which rows in the column are Inf or NaN...
            # (uses sapply due to limitations with is.infinite and data.frames)
            replace_index <- sapply(df[, i], 
                                    function(x) is.infinite(x) | is.na(x))
            # and replace them with NA
            df[, i][replace_index] <- NA
        }
        
        return(df)
    }
    
    # clean all the data frames with our Inf/NaN replacer
    cleaned_data <- lapply(cleaned_data, 
                           function(x) {
                               x <- replace_inf_nan(x)
                               return(x)
                           }
    )
    
    # return the cleaned data
    return(cleaned_data)
}