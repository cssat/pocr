#' Produces strings for stored procedure calls
#' 
#' @description
#' Every argument defaults to "0" for all. (Except date which defaults to 
#' "2008-01-01".) Arguments will be coerced to character, so plain sequences 
#' are fine.
#' 
#' Arguments described above as "matches" will be matched if a partial arg is 
#' given. For example, setting parameter \code{sp = "dis"} will be enough to 
#' find the discharge_exit_over_time stored procedure.
#' 
#' Vector arguments will be handled appropriately, so if you want to make a 
#' query for all 39 counties just use \code{county = 1:39}. Character counties 
#' will be converted to numerics, so specifying \code{county = "Kitsap"} or 
#' \code{county = c("king, pierce")} will also work (capitalization is ignored).
#'
#' @param sp (matches) Name of stored procedure (sans any "sp_" prefix or 
#' suffix such as "counts").
#' @param db (matches) Database to use. Defaults to "mysql" (equivalent to 
#' "test_annie"), the other option is "sqlserver" (equivalent to "poc").
#' @param date Point-in-time or cohort entry/exit date(s) here. (See also 
#' \code{\link{seq.Date}}.)
#' @param age age/age group code, see details.
#' @param ethnicity ?? hopefully a synonym for race??
#' @param county County code(s) OR character vectors with county names!
#' @param IHS IHS Service Codes
#' @param reporter code
#' @param access entry point (aka access type) code
#' @param allegation allegation code, 0: all, 1: PA, 2: SA, 3: Neglect, 4: any
#' @param finding code, 0: all, 1: PA, 2: SA, 3: Neglect, 4: any
#' @param gender 1 for Female, 2 for Male (0 for all).
#' @param initial_placement Initial placement setting, see details.
#' @param longest_placement longest placement setting, see details.
#' @param length_of_stay LOS code, 0: all, 1: > 7 days, 2: > 60 days, 
#' 3: > 1 year, 4: > 2 years
#' @param number_of_placements code
#' @param dependency Court Involvement/Dependency code
#' @param year Shouldn't matter to the database, but you can specify if you'd 
#' like
#'
#' @export
#' @examples
#' \dontrun{stored_procedure("poc1ab", date = seq(as.Date("2006-01-01"),
#'     as.Date("2009-01-01"), by = "year"), age = 1:3)}
stored_procedure <- function(sp = c("ia_trends_counts", "ia_trends_rates", 
                                    "ia_safety", "ooh_pit_counts", 
                                    "ooh_pit_rates", "ooh_flow_entries_counts",
                                    "ooh_flow_entries_rates", "ooh_flow_exits", 
                                    "ooh_reentry", "ooh_outcomes", 
                                    "ooh_outcomes_12m", "ooh_outcomes_24m",
                                    "ooh_outcomes_3m", "ooh_wb_familysettings", 
                                    "ooh_wb_siblings", "ooh_wb_siblings_pvt", 
                                    "ooh_wb_siblings_uncensored", 
                                    "population_household","population_person"
                                    ),
                  db = c("mysql", "sqlserver", "poc", "test_annie"),
                  date = 0,
                  age = 0,
                  ethnicity = 0,
                  county = 0,
                  IHS = 0,
                  reporter = 0,
                  access = 0,
                  allegation = 0,
                  finding = 0,
                  gender = 0,
                  initial_placement = 0,
                  longest_placement = 0,
                  length_of_stay = 0,
                  number_of_placements = 0,
                  dependency = 0,
                  year = 0){
    sp <- match.arg(sp)
    db <- match.arg(db)
    if (db == "poc") db <- "sqlserver"
    if (db == "test_annie") db <- "mysql"
    
    # if (is.character(county)) {
    #    county <- ref_lookup_county[tolower(ref_lookup_county[, 2]) %in% 
    #                                    tolower(county), 1]
    #}
    
    CALL <- mget(names(formals()), sys.frame(sys.nframe()))
    callArgs <- unlist(lapply(CALL, FUN= paste0, collapse = ","))
    argList <- list(ia = c("age", "ethnicity", "county", "reporter", "access", 
                           "allegation", "finding"),
                    ooh = c("age", "ethnicity", "gender", "initial_placement", 
                            "longest_placement", "county","length_of_stay", 
                            "number_of_placements", "IHS", "reporter", 
                            "access", "allegation", "finding", "dependency"),
                    population = c("county", "year")
    )
    if (str_detect(sp, "^ia")){
        sector <- "ia" 
    }
    
    if (str_detect(sp, "^ooh")){
        sector <-  "ooh"
    }
    
    if (str_detect(sp, "population")){
        sector <- "population"
    }
    
    sqlCall <- argList[[sector]]
    
    if (str_detect(sp, "^ia_trends") | 
        str_detect(sp, "^ooh_pit") | 
        str_detect(sp, "^ooh_flow") | 
        str_detect(sp, "ooh_wb_fam")){
        sqlCall <- c("date", sqlCall)
    }
    
    for (i in 1:length(callArgs)) {
        sqlCall <- str_replace(sqlCall, 
                               pattern = names(callArgs)[i], 
                               replacement = callArgs[i])
    }
    sqlCall <- paste0("'", sqlCall, "'", collapse = ",")
    if (db == "mysql") sqlCall <- paste0("(", sqlCall, ");")
    if (db == "mysql") sqlCall <- paste0("call sp_", sp, sqlCall)
    if (db == "sqlserver") sqlCall <- paste0("exec prod_sp_", 
                                             sp, " ", sqlCall)
    if (db == "test_annie") sqlCall <- paste0("test_annie.sp_", 
                                              sp, " ", sqlCall)
    return(sqlCall)
}
