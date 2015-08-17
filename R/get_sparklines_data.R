#' Helper function to get and clean County Dashboard trends data.
#' 
#' @description 
#' This function gets the data needed for the County Dashboard trends (aka -
#' the sparklines) and handles cleaning and preparation of the data.
#' 
#' @param annie_connection Active RODBC connection to one of the annie MySQL
#' servers.
#' 
#' @return
#' Return a single data frame with the base sparklines data. However, further 
#' work needs to be done with this data before it is ready for County
#' Dashboard consumption. This is handled by \code{update_county_dashbaord()}.
#' 
#' @export
get_sparklines_data <- function(annie_connection) {
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
                                            ...)
        )
        # correct the variable names
        names(target) <- tolower(make.names(names(target)))
        
        # return the target for filtering
        return(target)
    }
    
    # perform the needed stored procedure calls to get the base material
    # need to build the county and region trends data
    entry_counts <- query_wrapper(sp_name = "ooh_flow_entries_counts")
    entry_counts <- entry_counts %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        select(entry.cnt = number.of.entries, month, county_cd)
    
    entry_rates <- query_wrapper(sp_name = "ooh_flow_entries_rates")
    entry_rates <- entry_rates %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        select(entry.rate = rate.of.entries, month, county_cd)
    
    entry_0_to_4 <- query_wrapper(sp_name = "ooh_flow_entries_counts",
                                  age = 1)
    entry_0_to_4 <- entry_0_to_4 %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        select(entry.cnt.04 = number.of.entries, month, county_cd)
    
    entry_depend <- query_wrapper(sp_name = "ooh_flow_entries_counts",
                                  dependency = 2)
    entry_depend <- entry_depend %>% 
        filter(date_type == 2 & qry_type_poc1 == 2) %>%
        mutate(month = as.Date(cohort.period)) %>%
        select(entry.cnt.dep = number.of.entries, month, county_cd)
    
    placement_kin <- query_wrapper("ooh_wb_familysettings")
    placement_kin <- placement_kin %>% 
        filter(qry_type_poc1_first_all == 2) %>%
        mutate(month = as.Date(date)) %>%
        select(pit.kin = family.setting..kin.placement., month, county_cd) %>%
        mutate(year = as.integer(lubridate::year(month)))
    
    placement_counts <- query_wrapper("ooh_pit_counts")
    placement_counts <- placement_counts %>% 
        filter(date_type == 1 & qry_type_first_all == 2) %>%
        mutate(month = as.Date(date)) %>%
        select(pit.count = total.in.out.of.home.care.1st.day, month, county_cd)
    
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
    
    # build the base object
    trends_base <- expand.grid(county_cd = 0:39,
                               month = seq.Date(as.Date("2000-01-01"), 
                                                as.Date("2014-01-01"),
                                                by = "year"))
    
    # join the collected data to this object (plus ref_lookup_county_region,
    # built-in pocr data)
    trends <- trends_base %>%
        mutate(year = ifelse(lubridate::month(month) == 1, 
                             as.integer(lubridate::year(month)), NA)) %>%
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
        select(county, month, 
               agg.ent.rate, agg.ent.04, agg.ent.dep, agg.kin, agg.sibs)
    
    names(trends_county)[3:7] <- paste0("trend_", 0:4)
    
    # clean the data for regions
    trends_region <- trends %>% 
        mutate(region_6_tx = as.character(region_6_tx)) %>%
        filter(county_cd > 0) %>%
        group_by(region_6_tx, month) %>%
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
        select(region_6_tx, month,
               agg.ent.rate, agg.ent.04, agg.ent.dep, agg.kin, agg.sibs) %>%
        mutate(agg.ent.dep = ifelse(lubridate::year(month) < 2003, 
                                    NA, 
                                    agg.ent.dep))
    
    names(trends_region) <- c("id", "time", paste0("trend_", 0:4))
    
    # clean the data for state
    trends_state <- trends %>% filter(county_cd == 0) %>%
        group_by(month) %>%
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
        select(region_6_tx, month,
               agg.ent.rate, agg.ent.04, agg.ent.dep, agg.kin, agg.sibs) %>%
        mutate(agg.ent.dep = ifelse(lubridate::year(month) < 2003, 
                                    NA, 
                                    agg.ent.dep))
    
    names(trends_state) <- c("id", "time", paste0("trend_", 0:4))
    
    # collect and return the cleaned county, region, and state data
    
}