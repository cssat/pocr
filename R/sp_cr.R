#' Creates call for stored procedures
#'
#' @rdname sp_cr
#' 
#' @param sp stored procedure, default is for ia_trends_counts, use sp_names to find the names of all stored procedures
#' @param db choose database to contect to mysql, splserver, poc or test_annie
#' @param date
#' @param age
#' @param ethnicity 
#' @param county
#' @param IHS
#' @param reporter
#' @param access
#' @param allegation
#' @param finding
#' @param race
#' @param gender
#' @param initial_placement
#' @param longest_placement
#' @param length_of_stay
#' @param number_of_placements
#' @param service
#' @param budget
#' @param dependency
#' @param year
#' 
#' @return A call for stored procedures

sp_cr <- function(sp = c("ia_trends_counts", "ia_trends_rates", "ia_safety",
                         "ooh_pit_counts", "ooh_pit_rates", "ooh_flow_entries_counts",
                         "ooh_flow_entries_rates", "ooh_flow_exits", "ooh_reentry",
                         "ooh_outcomes", "ooh_outcomes_12m", "ooh_outcomes_24m",
                         "ooh_outcomes_3m", "ooh_wb_familysettings", "ooh_wb_siblings",
                         "ooh_wb_siblings_pvt", "ooh_wb_siblings_uncensored", "population_household",
                         "population_person"),
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
                  race = 0,
                  gender = 0,
                  intial_placement = 0,
                  longest_placement = 0,
                  length_of_stay = 0,
                  number_of_placements = 0,
                  service = 0,
                  budget = 0,
                  dependency = 0,
                  year = 0){
    sp <- match.arg(sp)
    db <- match.arg(db)
    if (db == "poc") db <- "sqlserver"
    if (db == "test_annie") db <- "mysql"
    
    if (is.character(county)) {
        county <- ref_lookup_county[tolower(ref_lookup_county[, 2]) %in% tolower(county), 1]
    }
    
    CALL <- mget(names(formals()), sys.frame(sys.nframe()))
    callArgs <- unlist(lapply(CALL, FUN= paste0, collapse = ","))
    argList <- list(ia = c("age", "ethnicity", "county", "reporter", "access", "allegation", "finding"),
                    ooh = c("age", "ethnicity", "gender", "intial_placement", "longest_placement", "county",
                            "length_of_stay", "number_of_placements", "IHS", "reporter", "access", "allegation",
                            "finding", "dependency"),
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
    
    if (str_detect(sp, "^ia_trends") | str_detect(sp, "^ooh_pit") | str_detect(sp, "^ooh_flow") | str_detect(sp, "ooh_wb_fam")){
        sqlCall <- c("date", sqlCall)
    }
    
    for (i in 1:length(callArgs)) {
        sqlCall <- str_replace(sqlCall, pattern = names(callArgs)[i], replacement = callArgs[i])
    }
    sqlCall <- paste0("'", sqlCall, "'", collapse = ",")
    if (db == "mysql") sqlCall <- paste0("(", sqlCall, ");")
    if (db == "mysql") sqlCall <- paste0("call sp_", sp, sqlCall)
    if (db == "sqlserver") sqlCall <- paste0("exec prod_sp_", sp, " ", sqlCall)
    if (db == "test_annie") sqlCall <- paste0("test_annie.sp_", sp, " ", sqlCall)
    return(sqlCall)
}

#' Clean up colnames from Portal, allow option to 
#' select columns and to convert to \code{Date} class.
#' @param df data.frame frame stored procedure
#' @param select optional character vector of columns to keep
#' @param date boolean indicating whether any \code{datetime} columns
#' @param date.type 1 is quarter and 2 is year
#' @param qrt.type
#' should be coverted to \code{Date} classes (default is \code{T}).
#' @export
cr_clean <- function(df, select = NULL, date = T,
                     date.type = 1, qry.type = "all.unique") {
    names(df) <- str_replace_all(names(df), pattern="&", replacement = "and")
    names(df) <- make.names(names(df), allow_ = F)
    names(df) <- tolower(names(df))
    names(df) <- str_replace_all(names(df), pattern=" |/|\\(|\\)", replacement = ".")
    names(df) <- str_replace_all(names(df), pattern="/", replacement = ".")
    names(df) <- str_replace_all(names(df), pattern="\\(", replacement = "")
    names(df) <- str_replace_all(names(df), pattern="\\)", replacement = "")
    
    qry.row.keep <- rep(TRUE, nrow(df))

    if (any(str_detect(names(df), "qry\\.type"))) {
        qry.type.col <- names(df)[str_detect(names(df), "qry\\.type")]
        if (qry.type == "all.unique") {
            if (any(df[, qry.type.col] == 2)) {
                qry.row.keep <- df[, qry.type.col] == 2
            } else {
                qry.row.keep <- df[, qry.type.col] == 0
                if (all(!qry.row.keep)) warning("No 'all' or 'unique' query type found.")
            } 
        } else {
            qry.row.keep <- df[, qry.type.col] == qry.type
        }
    }
    
    if ("date.type" %in% names(df)) {
        date.type.col <- names(df)[str_detect(names(df), "date\\.type")]
        if (any(df[, date.type.col] == 1)) {
            date.type.col <- df[, date.type.col] == 1
        } else {
            date.type.col <- df[, date.type.col] == 2
        }
    }
    
    df <- df[qry.row.keep & date.row.keep, ]
    
    uniques <- sapply(df, function(x) length(unique(x)))
    uniques <- names(uniques)[uniques > 1]
    if (! is.null(select)) {
        select <- make.names(select, allow_ = F)
        
    }
    
    df <- df[, unique(c(uniques, select))]
    
    if (date) {
        to.change <- which(sapply(df, is.POSIXct))
        for (i in to.change) {
            df[, i] <- as.Date(df[, i])
        }
    }
    return(df)
}

#' Stored Procedure Names
#' 
#' sp_names returns the nams of all stored procedures.

sp_names <- c("ia_trends_counts", "ia_trends_rates", "ia_safety",
              "ooh_pit_counts", "ooh_pit_rates", "ooh_flow_entries_counts",
              "ooh_flow_entries_rates", "ooh_flow_exits", "ooh_reentry",
              "ooh_outcomes", "ooh_outcomes_12m", "ooh_outcomes_24m",
              "ooh_outcomes_3m", "ooh_wb_familysettings", "ooh_wb_siblings",
              "ooh_wb_siblings_pvt", "ooh_wb_siblings_uncensored", "population_household",
              "population_person")
