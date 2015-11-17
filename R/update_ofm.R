

#' @title
#' Update the OFM tables in the POC SQL Server database.
#'
#' @description
#' Imports, cleans, and organizes the data needed for the OFM tables in the POC 
#' SQL server database. It then drops and recreates the tables with the cleaned 
#' data.
#' 
#' 
#' After downloading the data from the source, the data is imported as a list of
#' dataframes, one for each xlsx file. The columns of each dataframe are given 
#' arbitrary names. We then filter out irrelevant columns, such as area_id.
#' 
#' Each data frame needs to be filtered in a unique way:
#' 
#' -The total population counts dataframe (total) must include all counts, 
#'  except the actuall totals. Race based totals are kept for the construction 
#'  of the dataframe for other ethnic backgrounds.
#'  
#' -The hispanic population count dataframe (hispanic) must include only the 
#'  totals.
#'  
#' -The non-hispanic population count dataframe (non_hispanic) must include only
#'  black and white counts.
#'  
#' -Construction of the other dataframe: The count for other ethnic background
#' (others) is necessary. To construct the dataframe, all but the 
#' race based totals from total are filtered. The counts are derived by 
#' observing that
#'  
#'      others count = total count - hispanic count - non_hipanic black 
#'                     - non_hispanic white counts
#' 
#' The dataframes are not tidy: column headers contain the variables race and
#' gender. Hence, the dataframes are melted with the colvars: county, age and 
#' measurement_year (year). Then the new column is mutated into two seperate 
#' columns: race and gender. An additional column is added for source_census.
#' 
#' All data frames are filtered row-wise to remove irrelevant age groups and
#' totals.
#' 
#' Data is now tidy at this point, but data formatting begins. The counts are 
#' rounded and values are replaced by reference codes. Age groups are given in 
#' an inconsistent manner, so the data is grouped to get the count for each
#' age group.
#' 
#' Once formatting is finished, rows corresponding to 2015 are created by using
#' data from 2014.
#' 
#' NOTE: Once 2015 data is available the code requires some upkeep.
#' 
#'
#' @param poc_connection Need to provide an active RODBC connection to the
#' POC SQL server. NOTE: It is assumed that the object passed in is meaningful.
#' @param start_year Optional parameter where the user can specify the start
#' year. NOTE: It is assumed that the start year given is meaningful. 
#' @param end_year Optional parameter where the user can specify the end
#' year. NOTE: It is assumed that the end year given is meaningful. 
#' @return
#' The function updates the POC SQL database and returns TRUE on success.
#'
#' @export
update_ofm <- function(poc_connection, 
                       start_year = 2000,
                       end_year = 2014) {
    
    # test here for possible input problems
    # we cannot catch all of the errors so it is assumed that users will input 
    # relevant and meaningful objects.
    
    # Setting the default poc_connection to null allows us to check if something
    # is given
    poc_connection = validate_RODBC_input(poc_connection)
    if(!poc_connection$test_result){
        stop(poc_connection$test_message)
    }
    poc_connection = poc_connection$connection
    
    # It is likely that the data is from the year 2000 and up
    if(start_year < 2000 | end_year < 2000){
        stop("Any year specified must be 2000 or greater")
    }
    # Allowing the start year to be greater than the end year violates the
    # purpose of the variables
    if(start_year > end_year ){
        stop("Start year cannot be greater than end year")
    }
    
    # create year collection
    years <- as.character(start_year:end_year)
    
    # create a collection of files to acquire - this collection will be
    # continuously updated and will eventually contain the desired clean
    # data
    ofm_file_names <- list("hispanic" = 'sade_county10_h_5y_s1.xlsx',
                           "non_hispanic" = 'sade_county10_n_5y_s1.xlsx')
    
    # download the most current versions of the the OFM files
    test <-   lapply(ofm_file_names, 
                     function(x) {
                         download.file(paste0("http://www.ofm.wa.gov/pop/asr/sade/",
                                              x),
                                       destfile = x,
                                       mode = "wb")
                     }
    )
    
    # get the raw data from the xlsx files - this requires reading
    # multiple sheets from each file; returns a list of dataframes 
    ofm_collection <- lapply(ofm_file_names, function(x) {
        returned_list <- vector("list" , length(years))
        names(returned_list) <- years
        for(i in 1:length(years)){
            returned_list[[ i ]] <- as.data.frame(
                read_excel(x, 
                           sheet = years[ i ], 
                           col_names = FALSE,
                           skip = 4))
            returned_list[[ i ]]$sheetName = years[ i ]
        }
        return(returned_list)     
    }
    )
    
    # clean out the downloaded files
    lapply(ofm_file_names, unlink)
    
    # we want to bind the dataframes in the list together
    ofm_collection <- lapply(ofm_collection, function(x) do.call(rbind, x))
    
    # All of the df's have the same sort of columns. We use the symbols below 
    # for column names. A_ID is Area_id. The column names that use only two 
    # letters refer to columns names that contain values for two variables. The 
    # character at the starting index of these names refers to race and the last 
    # character in the name refers to sex. 
    # So, for example, T_T refers to a column of counts that are asscoiated with
    # rows with a race value of total and a gender value of total. They need to
    # be split later. Here are race codes for this set of names 
    # (gender is obvious).
    
    # A = Asian | B = Black | W = White | T = Total Race | N = Native |
    # P = Pacific Islander/Hawian | M = Mixed Race
    
    col_names <- c("county_desc", "A_ID", "age_group", "T_T", "T_M", "T_F",
                   "W_T" , "W_M" , "W_F" , "B_T" , "B_M" , "B_F",
                   "N_T" , "N_M" , "N_F" , "A_T" , "A_M" , "A_F",
                   "P_T" ,"P_M" , "P_F" ,"M_T" , "M_M" , "M_F","Year")
    
    # set col names
    ofm_collection <- lapply(ofm_collection, function(x){
        colnames(x) <- col_names
        return(x)
    }
    ) 
    
    # We need to manipulate the df's individually, since we need different 
    # things from each.
    
    
    # The only thing neeced from the Hispanic or Latino Population is Total for Male and Female
    # we also need to aggregate 15, 16 and 17 together
    
    ofm_collection[[1]] <- select(ofm_collection[[1]], Year, county_desc, age_group, T_M, T_F) %>%
        filter(age_group %in% c('0-4', '5-9', '10-14', '15', '16', '17') & county_desc != 'Washington State') %>%
        reshape2::melt(id = c("county_desc", "age_group", "Year"), 
                       variable = "column",
                       value.name = "pop_cnt") %>%
        mutate(cd_gndr = ifelse(substring(column, 3) == 'F', 1, 2),
               cd_race = 5,
               year = Year) %>%
        select(year, county_desc, cd_gndr, cd_race, age_group, pop_cnt)
    
    hisp_15_17 <- filter(ofm_collection[[1]], age_group %in% c(15, 16, 17)) %>%
        group_by(year, county_desc, cd_gndr, cd_race) %>%
        summarize(pop_cnt = sum(pop_cnt)) %>%
        mutate(age_group = '15-17') %>%
        select(year, county_desc, cd_gndr, cd_race, age_group, pop_cnt)
    
    ofm_collection[[1]] <-filter(ofm_collection[[1]], age_group %in% c('0-4', '5-9', '10-14'))
    
    # Getting totals for other groups
    # we also need to aggregate 15, 16 and 17 together 
    
    ofm_collection[[2]] <- select(ofm_collection[[2]], -starts_with('T'), -ends_with('T'), -A_ID) %>%
        filter(age_group %in% c('0-4', '5-9', '10-14', '15', '16', '17') & county_desc != 'Washington State') %>%
        reshape2::melt(id = c("county_desc", "age_group", "Year"), 
                       variable = "column",
                       value.name = "pop_cnt") %>%
        mutate(cd_gndr = ifelse(substring(column, 3) == 'F', 1, 2),
               cd_race = ifelse(substring(column, 1, 1) == "A", 2,
                                ifelse(substring(column, 1, 1) == "B", 3,
                                       ifelse(substring(column, 1, 1) == "W", 5,
                                              ifelse(substring(column, 1, 1) == "N", 1,
                                                     ifelse(substring(column, 1, 1) == "P", 4,
                                                            ifelse(substring(column, 1, 1) == "M", 7, 8)))))),
               year = Year) %>%
        select(year, county_desc, cd_gndr, cd_race, age_group, pop_cnt)
    
    non_hisp_15_17 <- filter(ofm_collection[[2]], age_group %in% c(15, 16, 17)) %>%
        group_by(year, county_desc, cd_gndr, cd_race) %>%
        summarize(pop_cnt = sum(pop_cnt)) %>%
        mutate(age_group = '15-17') %>%
        select(year, county_desc, cd_gndr, cd_race, age_group, pop_cnt)
    
    ofm_collection[[2]] <-filter(ofm_collection[[2]], age_group %in% c('0-4', '5-9', '10-14'))
    
    # the 4 datasets now need to be put together
    
    raceeth <- rbind(ofm_collection[[1]], ofm_collection[[2]], hisp_15_17, non_hisp_15_17)
    
    # round date to whole numbers
    
    raceeth[[6]] <- round(raceeth[[6]])
    
    # change age grouping to correct poc codes
    
    
    raceeth[[5]] <- ifelse(raceeth[[5]] == '0-4', 1,
                           ifelse(raceeth[[5]] == '5-9', 2,
                                  ifelse(raceeth[[5]] == '10-14', 3, 4)))
    
    # tries to drop the previous table if it exists
    try(sqlDrop(channel = poc_connection,
                sqtable = "public_data.mb_census_population"))
    
    # sends the df to sql
    sqlSave(channel = poc_connection, dat = raceeth,
            tablename = "public_data.mb_census_population",
            rownames = FALSE, colnames = FALSE)
    
    # the end result
    return("TRUE")
}