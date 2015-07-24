
cr_clean <- function(df, select = NULL, date = T,
                     date.type = 1, qry.type = "allunique") {
    
    names(df) <- str_replace_all(names(df), pattern="&", replacement = "and")
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
    uniques <- sapply(df, function(x) length(unique(x)))
    
    
    if (date) {
        to.change <- which(sapply(df, is.POSIXct))
        for (i in to.change) {
            df[, i] <- as.Date(df[, i])
        }
    }
    return(df)
}

?make.names














