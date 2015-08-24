#' @title
#' Update the OFM tables in the POC SQL Server database.
#'
#' @description
#' Imports, cleans, and organizes the data needed for the OFM tables in the POC 
#' SQL server database. It then drops and recreates the tables with the cleaned 
#' data.
#' 
#' HOW IT WORKS
#' 
#' We first download the data from the source. We then use a helper function
#' to import the data as a list of dataframes, one for each xlsx file.
#' We then filter out irrelevant rows and columns. Since some columns of
#' the xlsx file represent values, we melt. Moreover, the values of the column
#' take on values for two separate variables. Thus we mutate to split the 
#' columns. Once the tidying of the data is done, we create another data frame 
#' which initially takes on race-based totals. These totals are used to compute
#' the calculated other count which is given by:
#' 
#' others = total - hispanic - black - white
#' 
#' We then combine all of the tables and then create rows for 2015. 2015 data
#' will be replicated from the 2014 data.
#' 
#' Once we have 2014 data 
#'
#' @param poc_connection Need to provide an active RODBC connection to the
#' POC SQL server.
#' @param start_year Optional parameter where the user can specify the start
#' year.
#' @param end_year Optional parameter where the user can specify the end
#' year.
#' @return
#' The function updates the POC SQL database and returns TRUE on success.
#'
#' @export
update_ofm <- function(poc_connection, 
                       start_year = 2000, end_year = 2014) {
    # create year collection
    years <- as.character(start_year:end_year)
    
    # create a collection of files to acquire - this collection will be
    # continuously updated and will eventually contain the desired clean
    # data
    ofm_file_names <- list("total" = 'sade_county10_t_5y_s1.xlsx',
                           "hispanic" = 'sade_county10_h_5y_s1.xlsx',
                           "non_hispanic" = 'sade_county10_n_5y_s1.xlsx')
    
    # download the most current versions of the the OFM files
    lapply(ofm_file_names, 
           function(x) {
               download.file(paste0("http://www.ofm.wa.gov/pop/asr/sade/",
                                    x),
                             destfile = x,
                             mode = "wb")
           }
    )
    
    # get the raw data from the xlsx files - this requires reading
    # multiple sheets from each file; import_xlsx returns a list of
    # dataframes (each sheet becoming a dataframe)
    ofm_collection <- lapply(ofm_file_names, 
                             function(x) {
                                 import_xlsx(x,
                                             years,
                                             col_sheets = TRUE,
                                             col_names = FALSE,
                                             # first rows are junk
                                             skip = 4)
                             }
    )
    
    # clean out the downloaded files
    lapply(ofm_file_names, unlink)
    
    # we want to bind the dataframes in the list together
    ofm_collection <- lapply(ofm_collection, function(x) do.call(rbind, x))
    
 
    #All of the df's have the same sort of columns. We use the symbols
    #below for column names. A_ID is Area_id. The column names that
    #use only two letters refer to columns names that contain two variables.
    #The character at the starting index of these names refers to race 
    #and the last character in the name refers to sex.
    # So, for example, T_T refers to a column for that gives a count for
    #Total race and Total gender.
    #A=Asian | B=Black | W=White | T=Total Race | N=Native |
    #P=Pacific Islander/Hawian | M=Mixed Race
    # NOTE: THE RACE IDENTIFIERS ABOVE MAY NEED TO BE CHANGED 
    # SO THAT IT MATCHES REFERENCE TABLES
    col_names <- c("county", "A_ID", "age_group", "T_T", "T_M", "T_F",
                   "W_T" , "W_M" , "W_F" , "B_T" , "B_M" , "B_F",
                   "N_T" , "N_M" , "N_F" , "A_T" , "A_M" , "A_F",
                   "P_T" ,"P_M" , "P_F" ,"M_T" , "M_M" , "M_F","Year")
    
    
    #set col names
    ofm_collection <- lapply(ofm_collection, function(x){
        colnames(x) <-   col_names
        return(x)}) 
    
    #We need to manipulate the df's individually, since we need 
    #different things from each.
    
    #No totals whatsoever in total df
    #(we do need to preserve race based totals for the other df).
    ofm_collection[[1]] <- select(ofm_collection[[1]], -ends_with("T"), -A_ID)
    
    #Only race based totals in hisp df.
    ofm_collection[[2]] <- select(ofm_collection[[2]], county, age_group, Year,
                                  T_M, T_F)
    
    #Only white and black data from non hisp df.
    ofm_collection[[3]] <- select(ofm_collection[[3]], county, age_group, Year,
                                  B_M, B_F, W_M, W_F)
    
    #melt all of the dfs because race/gender are values
    # NOTE: age_group
    # MAY NEED TO BE CHANGED SO THAT IT MATCHES REFERENCE TABLES
    ofm_collection <- lapply(ofm_collection, function(x){
        
        x <- melt(x, id = c("county", "age_group", "Year"), 
                  variable = "column",
                  value.name = "Estimated_Pop")
        
        return(x)
        })
    
    #mutate to split columns since race and gender share only one column
    #we also create a new column for source census
    # NOTE: race_group and gender
    # MAY NEED TO BE CHANGED SO THAT IT MATCHES REFERENCE TABLES
    ofm_collection <- lapply(ofm_collection, function(x){
        x <- mutate(x, gender = substring(column,3),
                    race_group = substring(column,1,1),
                    source_census = Year)
        
        return(x)
        }) 
    
    #filter out rows that are irrelevant
    ofm_collection <- lapply(ofm_collection,   function(x){
        
        x <- filter(x, county != "Washington State", 
                    age_group %in% c("0-4", "5-9", "10-14", "15", "16", "17"))
        
        return(x)
    })
    
    #begin creating the other df (the reason we needed totals previously).
    #We take the rows that are counts of race totals from the total df.
    #Then change the name to OEB (other ethnic background) so that we 
    #can differentiate the races in this table from the remaining ones.
    #Eventually we need to replace the counts with a calculated other count
    # NOTE: OEB MAY NEED TO BE CHANGED SO THAT IT MATCHES REFERENCE TABLES
    ofm_collection[[4]] <- filter(ofm_collection[[1]], race_group == "T")%>%
                                  mutate(race_group="OEB")%>%
                                  select(-column)
    
    #We need to refilter each table separately 
    #No longer need totals
    ofm_collection[[1]] <- filter(ofm_collection[[1]], race_group!="T")%>%
                                  select(-column)
    #Change the race from total to hispanic
    ofm_collection[[2]] <- mutate(ofm_collection[[2]], race_group="H")%>%
                                  select(-column)
    #We need to differentiate between non-hisp 
    #black/white and totals df black/white
    ofm_collection[[3]] <- filter(ofm_collection[[3]],
                                  race_group=="B" | race_group=="W")%>%
                                  mutate(race_group=
                                         ifelse(race_group=="B","NHB","NHW"))%>%
                                  select(-column)

    #round populations as is done in the the excel files
    ofm_collection <- lapply(ofm_collection, function(x){
        
        x$Estimated_Pop <-as.integer(round(x$Estimated_Pop))
        
        return(x)
        })
    
    #join on county, race, age, sex
    ofm_collection <- lapply(ofm_collection, function(x){
        
        x <- inner_join(x,ref_lookup_county, by="county")%>%
             inner_join(ref_lookup_race, by="race_group")%>%
             inner_join(ref_lookup_age, x, by="age_group")%>%
             inner_join(ref_lookup_gender, x, by="gender")%>%
             select(-county, -region_cd, -race_group, 
                    -race, -age_group, -gender)
        
        return(x)
        })
 
    
    #age groups are ready to be boiled down to 4
    ofm_collection <- lapply(ofm_collection, function(x){
        
        x <- ungroup(group_by(x, county_cd, age_grouping_cd, Year,
                              pk_gndr, cd_race, source_census)%>%
                         summarize(Estimated_Pop = sum(Estimated_Pop)))
    })
    
    #arrange dfs 
    ofm_collection <- lapply(ofm_collection, function(x){
        
        x <- x[c("source_census", "county_cd", "pk_gndr", "cd_race", 
                 "age_grouping_cd", "Year", "Estimated_Pop")]
    })
    
    #we need these to compute the calculated other count
    #12 is nonhispanic black and 11 is nonhispanic white
    tempB <- filter(ofm_collection[[3]], cd_race == 12)
    tempW <- filter(ofm_collection[[3]], cd_race == 11)
    tempH <- ofm_collection[[2]]
    
    #we need to join the counts so we need distinct col names
    colnames(tempB) <- c("source_census","county_cd", "pk_gndr", "cd_raceB", 
                         "age_grouping_cd", "Year", "popB")
    colnames(tempW) <- c("source_census","county_cd", "pk_gndr", "cd_raceW", 
                         "age_grouping_cd", "Year", "popW")
    colnames(tempH) <- c("source_census","county_cd", "pk_gndr", "cd_raceH", 
                         "age_grouping_cd", "Year", "popH")
    
    #Join the columns so that we can compute
    ofm_collection[[4]] <- left_join(ofm_collection[[4]],
                                    tempB, 
                                    by = c("source_census", "county_cd", 
                                           "pk_gndr", "age_grouping_cd", 
                                           "Year"))%>%
                           left_join(tempW, 
                                    by = c("source_census", "county_cd",
                                           "pk_gndr", "age_grouping_cd",
                                           "Year"))%>%
                           left_join(tempH,
                                    by= c("source_census","county_cd", 
                                          "pk_gndr", "age_grouping_cd",
                                          "Year"))
    
    #Create a new column that has the calulated other count
    #count = total-black-white-hispanic
    ofm_collection[[4]] <- mutate(ofm_collection[[4]], 
                                  Pop = Estimated_Pop
                                  -popH
                                  -popB
                                  -popW)
    
    #Set the new values into the appropriate column,
    #which preserves column order
    ofm_collection[[4]]$Estimated_Pop <- ofm_collection[[4]]$Pop
    
    #remove irrelevant columns
    ofm_collection[[4]] <- select(ofm_collection[[4]],
                                  source_census, county_cd,
                                  pk_gndr, cd_race, 
                                  age_grouping_cd, Year, Estimated_Pop)
    
    #clear up space by remioving temp files
    rm(tempH, tempW, tempB)
    
    #Begin creating the actuall output
    ofm_collection[[5]] <- rbind(ofm_collection[[1]], ofm_collection[[2]],
                                 ofm_collection[[3]], ofm_collection[[4]])
    
    #make sure integers are integers
    ofm_collection[[5]]$Year <- as.integer(ofm_collection[[5]]$Year)
    ofm_collection[[5]]$source_census <-
        as.integer(ofm_collection[[5]]$source_census)
    
    #create the 2015 counts
    temp_2014 <- filter(ofm_collection[[5]], Year == 2014)
    temp_2014$source_census <- 2015
    temp_2014$Year <- 2015
    ofm_collection[[5]] <- rbind(ofm_collection[[5]], temp_2014)
    
    #give formal names
    colnames(ofm_collection[[5]]) <-c("source_census", "county_cd",
                                      "pk_gndr", "cd_race", "age_grouping_cd",
                                      "measurement_year", "pop_cnt")
    
    
    return(ofm_collection[[5]])
}
import_xlsx <- function( fileName , sheetNames = c( "" ), col_sheets = TRUE,
                         ... ){
    
    returnedDatalist <- vector("list" , length( sheetNames ))
    
    names( returnedDatalist ) <- sheetNames
    
    for(i in 1 : length( sheetNames ) ){
        returnedDatalist[[ i ]] <- as.data.frame( 
            read_excel( fileName , sheet = sheetNames[ i ], 
                        ... ) )
        if(col_sheets == TRUE) returnedDatalist[[ i ]]$sheetName = sheetNames[i]
    }
    return( returnedDatalist )
}