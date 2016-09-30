#' Helper function to get and clean County Dashboard "fast facts" data.
#' 
#' @description 
#' This function gathers the base data needed for the County Dashboard fast 
#' facts and handles initial cleaning and preparation of this data. 
#' 
#' Once finished, the fast facts data - along with the sparklines data
#' gathered by \code{get_sparklines_data()} - is handed off to 
#' \code{finalize_dashboard_data()} to put it in the final format needed
#' for app consumption.
#' 
#' @param poc_connection Active RODBC connection to the POC SQL server.
#' 
#' @return
#' Returns a list with three data frames - county, region, and state - needed
#' to support the different fast fact views. These data frames need further
#' processing prior to app consumption.
#' 
#' @export
get_fast_facts <- function(poc_connection) {
    # most of the work done to create the fast facts data is handled via
    # SQL queries and relies heavily on the creation and manipulation of a
    # temp table #dashboard_fast_facts
    
    # we create our temp table
    # NOTE: temp tables are dropped when the creating connection closes and
    #       closing the connection when done is good practice (handled by
    #       update_county_dashboard or by the user if get_fast_facts is 
    #       called outside the wrapper)
    sqlQuery(poc_connection,
             "
             select 
                rlcr.county,
                region_6_tx,
                pop_under_18,
                pop_under_5,
                unemp_rate,
                seniors,
                grad_rate,
                enrollment,
                app_rate
             into #dashboard_fastfacts
             from
             (select
                rlcp1.county_cd,
                sum(rlcp1.pop_cnt) pop_under_18
             from ref_lookup_census_population rlcp1
             where source_census = 2013 
                and cd_race < 9
             group by county_cd) t1
             left join
             (-- pop under 5
             select
                county_cd,
                sum(pop_cnt) pop_under_5
             from ref_lookup_census_population
             where source_census = 2013 
                and cd_race < 9
                and age_grouping_cd = 1
             group by county_cd) t2
             on t1.county_cd = t2.county_cd
             left join
             (select 
                county, 
                county_cd, 
                region_6_cd, 
                region_6_tx
             from dbo.ref_lookup_county_region) rlcr
             on t1.county_cd = rlcr.county_cd
             left join
             (-- unemployment
             select
                county,
                rate / 100 unemp_rate
             from [public_data].[unemployment]
             where date_type = 2
                and year(time) = 2012) t3
             on rlcr.county = t3.county
             left join
             (-- hs grad rate
             select
                county_cd,
                seniors,
                grads,
                grad_rate
             from public_data.hs_graduation) grad
             on t1.county_cd = grad.county_cd
             left join
             (-- free and reduced lunch
             select * 
             from public_data.free_lunch_county) frl
             on t1.county_cd = frl.county_cd;
             "
    )
    
    # we create our county fast facts
    county <- sqlQuery(poc_connection, 
                       "
                       select county id,
                            pop_under_18 pop_0,
                            pop_under_5 * 1.00 / pop_under_18 pop_1,
                            unemp_rate pop_2,
                            grad_rate pop_3,
                            app_rate pop_4
                       from #dashboard_fastfacts
                       ", 
                       stringsAsFactors = F
    )
    
    # and our region fast facts
    region <- sqlQuery(poc_connection, 
                       "
                       select region_6_tx id,
                            sum(pop_under_18) pop_0,
                            sum(pop_under_5) * 1.00 / sum(pop_under_18) pop_1,
                            sum(unemp_rate * pop_under_18) * 1.00 / sum(pop_under_18) pop_2,
                            sum(seniors * grad_rate) * 1.00 / sum(seniors) pop_3,
                            sum(app_rate * enrollment) / sum(enrollment) pop_4
                       from #dashboard_fastfacts
                       group by region_6_tx
                       ",
                       stringsAsFactors = F
    )
    
    # and our state fast facts
    state <- sqlQuery(poc_connection, 
                      "
                      select
                        'Washington' id,
                        sum(pop_under_18) pop_0,
                        sum(pop_under_5) * 1.00 / sum(pop_under_18) pop_1,
                        sum(unemp_rate * pop_under_18) * 1.00 / sum(pop_under_18) pop_2,
                        sum(seniors * grad_rate) * 1.00 / sum(seniors) pop_3,
                        sum(app_rate * enrollment) / sum(enrollment) pop_4
                      from #dashboard_fastfacts
                      ",
                      stringsAsFactors = F
    )
    
    # we gather our data frames 
    cleaned_data <- list("county" = county,
                         "region" = region,
                         "state" = state)
    
    # do a quick clean to insure that the number data types are consistent 
    # (numeric) as sometimes values get converted to int or other types
    # during the query process
    make_numeric <- function(df) {
        # for each column...
        for(i in 1:ncol(df)) {
            # if it's NOT character...
            if(typeof(df[, i]) != "character") {
                # make it numeric
                df[, i] <- as.numeric(df[, i])
            }
        }
        
        # return the updated df
        return(df)
    }
    
    cleaned_data <- lapply(cleaned_data, make_numeric)
    
    # also correct the variable names from "pop_0" to "population_0" etc.
    cleaned_data <- lapply(cleaned_data,
           function(x) {
               names(x) <- c("id", paste0("population_", 0:4))
               return(x)
           }
    )
    
    # return the cleaned data
    return(cleaned_data)
}
