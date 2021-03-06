#' Clean up colnames from Portal, allow option to select columns and to 
#' convert to \code{Date} class.
#' 
#' @param df data.frame frame stored procedure
#' @param select optional character vector of columns to keep
#' @param date boolean indicating whether any \code{datetime} columns
#' @param date.type 1 is quarter and 2 is year
#' @param qry.type should be converted to \code{Date} classes (default is 
#' \code{T}).
#' 
#' @export
cr_clean <- function (df, select = NULL, date = TRUE, date.type = 1, qry.type = "all.unique") 
{
    names(df) <- str_replace_all(names(df), pattern = "&", replacement = "and")
    names(df) <- make.names(names(df), allow_ = F)
    names(df) <- tolower(names(df))
    names(df) <- str_replace_all(names(df), pattern = " |/|\\(|\\)", 
                                 replacement = ".")
    names(df) <- str_replace_all(names(df), pattern = "/", replacement = ".")
    names(df) <- str_replace_all(names(df), pattern = "\\(", 
                                 replacement = "")
    names(df) <- str_replace_all(names(df), pattern = "\\)", 
                                 replacement = "")
    
    qry.row.keep <- rep(TRUE, nrow(df))
    
    if (any(str_detect(names(df), "qry\\.type"))) {
        qry.type.col <- names(df)[str_detect(names(df), "qry\\.type")]
        if (qry.type == "all.unique") {
            if (any(df[, qry.type.col] == 2)) {
                qry.row.keep <- df[, qry.type.col] == 2
            }
            else {
                qry.row.keep <- df[, qry.type.col] == 0
                if (all(!qry.row.keep)) 
                    warning("No 'all' or 'unique' query type found.")
            }
        }
        else {
            qry.row.keep <- df[, qry.type.col] == qry.type
        }
    }
    
    if (date & "date.type" %in% names(df)) {
        date.type.col <- names(df)[str_detect(names(df), "date\\.type")]
        if (date.type == 0) {
            date.row.keep <- df[, date.type.col] == 0
        }
        if (date.type == 1) {
            date.row.keep <- df[, date.type.col] == 1
        }
        if (date.type == 2) {
            date.row.keep <- df[, date.type.col] == 2
        }
        df <- df[qry.row.keep & date.row.keep, ]
    } else {
        df <- df[qry.row.keep, ]
    }
    
    uniques <- sapply(df, function(x) length(unique(x)))
    uniques <- names(uniques)[uniques > 1]
    if (!is.null(select)) {
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