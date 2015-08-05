###############################################################################
## deprecations.R

# This is a collection of functions that have been renamed or replaced in the
# current version of pocr. 

# These functions will be removed in future pocr versions but are retained
# here with deprecation warnings so that dependencies can be identified and 
# corrected without code breaking everywhere.

# It is the responsibility of the updater to insure that updated functions
# will accept the old function arguments during the deprecation cycle. 

# Alternitavely, you can:
# - give the updated version a new name
# - treat the old version as a deprecation due to removal
# - rename the updated version after the deprecation cycle and use the 
#   deprecation due to rename process to direct people back to the original name

# EXAMPLE OF DEPRECATION DUE TO RENAME:
# oldVersion <- function(...) {
#     warning("
#             'oldVersion is deprecated as of pocr VX.X and will be removed in 
#             future versions of pocr. Use 'new_version' instead.
#             ")
#     
#     # arguments passed to updated function
#     new_version(...)
# }

# EXAMPLE OF DEPRECATION DUE TO REMOVAL:
# function_to_remove <- function(args) {
#     warning("
#             'function_to_remove' is no longer supported as of pocr VX.X and
#             will be removed in future versions of pocr. Please check the 
#             package documentation for alternative solutions or contact the
#             package administration for support - use 'maintainer('pocr')' to
#             get contact details.")
#             
#             function_code...
# }

###############################################################################

# trend_plot and context_plot were completely replaced with updated versions,
# no intermediate steps

# trend_plot2, trend_plot3, and trend_plot_state were removed from the
# package, no intermediate steps

# stored_procedure was updated with current sp_cr code, sp_cr was given the
# old stored procedure code and is being treated as a rename

# read.sql renamed to read_sql
read.sql <- function(...) { 
    warning("
            'read.sql' is deprecated as of pocr V1.0 and will be removed in
            future versions of pocr. Use 'read_sql' instead.
            ")
    
    # arguments passed to updated function
    read_sql(...)
}

# fac.to.num renamed to factor_to_number
fac.to.num <- function(...) {
    warning("
            'fact.to.num' is deprecated as of pocr V1.0 and will be removed in
            future versions of pocr. Use 'factor_to_number' instead.
            ")
    
    # arguments passed to updated function
    factor_to_number(...)
}

# layOut renamed to lay_out
layOut <- function(...) {
    warning("
            'layOut' is deprecated as of pocr V1.0 and will be removed in
            future versions of pocr. Use 'lay_out' instead.
            ")
    
    # arguments passed to updated function
    lay_out(...)
}

# treat as renamed to stored_procedure
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
sp_cr <- function(sp = c("poc1ab", "poc2", "poc3",
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
    warning("
            'sp_cr' (formerly stored_procedure) is deprecated as of pocr V1.0 
            and will be removed in future versions of pocr. Use 
            'stored_procedure' instead.
            ")
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

# remove due to lack of use
#' Mode of a vector
#' 
#' Found on Stackoverflow.
#' 
#' @param x Vector for which you want to know the mode.
#' @export
Mode <- function(x) {
    warning("
            'Mode' is no longer supported as of pocr V1.0 and
            will be removed in future versions of pocr. Please check the 
            package documentation for alternative solutions or contact the
            package administration for support - use 'maintainer('pocr')' to
            get contact details.")
    
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# remove
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

# remove (documentation for sp_names)
#' Stored Procedure Names
#' @description sp_names returns the nams of all stored procedures.
#' @docType data
NULL

# remove: outdated cr_clean
#' Clean up colnames from Portal, allow option to 
#' select columns and to convert to \code{Date} class.
#' @param df data.frame frame stored procedure
#' @param select optional character vector of columns to keep
#' @param date boolean indicating whether any \code{datetime} columns
#' should be coverted to \code{Date} classes (default is \code{T}).
#' @export
portal_clean <- function(df, select = NULL, date = T) {
    names(df) <- make.names(names(df, allow_ = F))
    names(df) <- tolower(names(df))
    names(df) <- str_replace_all(names(df), pattern=" |/|\\(|\\)", replacement = ".")
    names(df) <- str_replace_all(names(df), pattern="/", replacement = ".")
    names(df) <- str_replace_all(names(df), pattern="\\(", replacement = "")
    names(df) <- str_replace_all(names(df), pattern="\\)", replacement = "")
    if (! is.null(select)) {
        select <- make.names(select, allow_ = F)
        df <- df[, select]
    }
    if (date) {
        to.change <- which(sapply(df, is.POSIXct))
        for (i in to.change) {
            df[, i] <- as.Date(df[, i])
        }
    }
    return(df)
}

# REMOVE
#' Sets working directory to \code{"S:/Data Portal"}
#' @export
resetwd <- function() {
    setwd("S:/Data Portal")
}