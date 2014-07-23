#' Produces strings for stored procedure calls
#' 
#' NEEDS UPDATING
#' 
#' Every argument defaults to "0" for all. (Except date which defaults to "2008-01-01".)
#' Arguments will be coerced to character, so plain sequences are fine.
#' 
#' Arguments described above as "matches" will be matched if a partial arg is given.
#' For example, setting parameter \code{sp = "dis"} will be enough to find the
#' discharge_exit_over_time stored procedure.
#' 
#' Vector arguments will be handled appropriately, so if you want to make a query
#' for all 39 counties just use \code{county = 1:39}. Character counties will be
#' converted to numerics, so specifying \code{county = "Kitsap"} or \code{county = c("king, pierce")}
#' will also work (capitalization is ignored).
#' 
#' On TO-DO list: make it smarter about recognizing text office and county groups.
#'
#' @param sp (matches) Name of stored procedure (sans any "sp_" prefix or suffix such as "counts").
#' @param db (matches) Database to use. Defaults to "mysql" (equivalent to "test_annie"), the other option
#' is "sqlserver" (equivalent to "poc").
#' @param type (matches) Suffix of stored procedure (e.g. "counts", "perCapita", "MR", or "kin").
#' @param date Point-in-time or cohort entry/exit date(s) here. (See also \code{\link{seq.Date}}.)
#' @param age age/age group code, see details.
#' @param race race/ethnicity, see details.
#' @param gender 1 for Female, 2 for Male (0 for all).
#' @param init Initial placement setting, see details.
#' @param last Last placement setting, see details.
#' @param reason Removal reason.
#' @param county County code.
#' @param custody Custody removed from.
#' @param office Office code.
#' @param county_grp County Group Code.
#' @param siblings Sibling group code 0: All, 1: 2 siblings, 2: 3 siblings, 3: 4 siblings, 4: >4 siblings.
#'
#' @export
#' @examples
#' \dontrun{storedProcedure("poc1ab", date = seq(as.Date("2006-01-01"),
#'     as.Date("2009-01-01"), by = "year"), age = 1:3)}
stored_procedure <- function(sp = c("poc1ab", "poc2", "poc3",
                                    "discharge_exit_over_time",
                                    "discharge_exit_over_time_mr",
                                    "PBCP5", "PBCS2", "PBCS3", "PBCW3", "PBCW4"),
                             db = c("mysql", "sqlserver", "poc", "test_annie"),
                             type = c("", "counts", "perCapita", "MR", "kin"),
                             date = "2008-01-01",
                             age = 0,
                             race = 0,
                             gender = 0,
                             init = 0,
                             last = 0,
                             reason = 0,
                             county = 0,
                             custody = 0,
                             office = 0,
                             county_grp = NA,
                             siblings = 0) {
    sp <- match.arg(sp)
    type <- match.arg(type)
    db <- match.arg(db)
    if (db == "poc") db <- "sqlserver"
    if (db == "test_annie") db <- "mysql"
    
    if (sp %in% c("poc1ab", "poc2", "poc3") & type %nin% c("counts", "perCapita")) {
        type <- "counts"
    }
    if (is.character(county)) {
        county <- ref_lookup_county[tolower(ref_lookup_county[, 2]) %in% tolower(county), 1]
    }
    
    CALL <- mget(names(formals()), sys.frame(sys.nframe()))
    callArgs <- unlist(lapply(CALL, FUN= paste0, collapse = ","))
    argList <- list(poc1ab = c("date","age","race","gender","init","last","reason","county","custody"),
                    poc2 = c("date","age","race","office"),
                    poc3 = c("date","age","race","office"),
                    discharge_exit_over_time = c("date","age","race","gender","init","last","reason","county"),
                    discharge_exit_over_time_mr = c("date","age","race","gender","init","last","reason","county"),
                    PBCP5 = c("date","age","race","gender","init","last","reason","county"),
                    PBCS2 = c("date","age","race","gender","office"),
                    PBCS3 = c("date","race","office"),
                    PBCW3 = c("date","age","race","gender","init","last","reason","county"),
                    PBCW4 = c("date","age","race","gender","init","last","reason","county","siblings")
    )
   
    sqlCall <- argList[[sp]]
    for (i in 1:length(callArgs)) {
        sqlCall <- str_replace(sqlCall, pattern = names(callArgs)[i], replacement = callArgs[i])
    }
    sqlCall <- paste0("'", sqlCall, "'", collapse = ",")
    if (db == "mysql") sqlCall <- paste0("(", sqlCall, ");")
    if (nchar(type) > 0) {
        sp <- paste(sp, type, sep = "_")
    }
    if (db == "mysql") sqlCall <- paste0("call sp_", sp, sqlCall)
    if (db == "sqlserver") sqlCall <- paste0("exec prod_sp_", sp, " ", sqlCall)
    return(sqlCall)
}


