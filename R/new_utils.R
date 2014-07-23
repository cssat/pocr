#' Function for reading in a SQL query.
#' 
#' This probably still needs some work.
#' 
#' @param filename You know what goes here
#' @param silent boolean on whether you want warnings ignored (TRUE).
#' @export
read.sql <- function(filename, silent = TRUE) {
    q <- readLines(filename, warn = !silent)
    q <- q[!grepl(pattern = "^\\s*--", x = q)] # remove full-line comments
    q <- sub(pattern = "--.*", replacement = "", x = q) # remove midline comments
    q <- paste(q, collapse = " ")
    return(q)
}

# ## read.sql test
# for(file in paste0("t", 1:9, ".sql")) {
#     cat("\nfile ", file, "\n")    
#     print(head(sqlQuery(con, read.sql(file))))
# }

#' Fills in NAs in the first argument from the second
#' @export
`%coalesce%` <- function(a, b) ifelse(is.na(a), b, a)

#' Tests (element-wise) if the first argument falls within
#' the range of the second.
#' @examples
#' 1 %btwn% c(0, 5)
#' 0:4 %btwn c(0.5, 3, 3.5)
#' @export
`%btwn%` = function(a, b) a>= min(b) & a <= max(b)


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


#' Factor to numeric via character
#' @param x factor to be converted to numeric
#' @export
fac.to.num <- function(x) {
    if (! is.factor(x)) warning("Didn't start as a factor (fac.to.num).")
    return(as.numeric(as.character(x)))
}

#' Arranging ggplots
#' 
#' Provides a \code{layout}-like interface for arranging ggplots of different sizes.
#' 
#' @param ... Each argument should be of the form \code{list(plot, rows, columns)},
#' where \code{plot} is a ggplot (or similar), and \code{rows} and \code{columns}
#' are consecutive sequences indicating the row and column numbers for \code{plot}
#' to span.
#' 
#' @author Alan D. Jassby and James E. Cloern (originally from the \code{wq} package).
#' 
#' @examples
#' \dontrun{
#' gg <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
#' layOut(list(gg, 1:2, 1:3),
#'        list(gg, 3, 1:2),
#'        list(gg, 3, 3))
#' }
#' @export
layOut = function(...) {
    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))    
    
    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]], 
                                         layout.pos.col = x[[i]][[3]]))
    }
} 


#' Returns an appropriate function to pass to the \code{breaks} argument
#' of any \code{continuous_scale}
#' 
#' @param n suggested number of breaks (defaults to 5)
#' @export
#' @examples
#' ggplot(mtcars, aes(x = disp, y = mpg)) +
#' geom_point() +
#'     scale_y_log10()
#'     
#'  ggplot(mtcars, aes(x = disp, y = mpg)) +
#'      geom_point() +
#'          scale_y_log10(breaks = break_setter())
#'          
#'  ggplot(mtcars, aes(x = disp, y = mpg)) +
#'      geom_point() +
#'      scale_y_log10(breaks = break_setter(10))
#' @export
break_setter = function(n = 5) {
    function(lims) {pretty(x = as.numeric(lims), n = n)}
}

#' POC theme for ggplot
#' 
#' Based off of \code{theme_bw}, this frames the plot in POC blue,
#' eliminated minor grid lines, and will try to match to a POC font.
#' There are also some other options.
#' 
#' @param font Partial-matches to "Arial", "PT Sans" (for web) or "Frutiger LT Std 45 Light" (for print).
#' @param slant boolean for whether x-axis labels should be slanted. (FALSE)
#' @param gridlines boolean for whether there should be any gridlines (TRUE)
#' @param expand.margin boolean for whether the right margin needs padding. Useful if \code{slant}
#' is \code{TRUE} and the labels on the right side are lengthy.
#' @export
theme_poc <- function(
    font = c("Arial", "PT Sans", "Frutiger LT Std 45 Light"),
    slant = FALSE,
    gridlines = TRUE,
    expand.margin = FALSE,
    ...) {
    font <- match.arg(font)
    if (font %nin% fonts()) {
        warning("Looks like you don't have ", font, " installed. Attempting to use Arial.")
        font <- "Arial"
    }
    poc.theme <- theme_bw(..., base_family = font) +
        theme(panel.border = element_rect(size = 1, colour = poc_colors[1]),
              axis.ticks = element_line(size = 1, colour = poc_colors[1]),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = poc_colors[1]),
              strip.text.x = element_text(colour = "white"),
              strip.text.x = element_text(colour = "white"))
    if (slant) poc.theme <- poc.theme + theme(axis.text.x = element_text(angle = -25, hjust = 0))
    if (expand.margin) poc.theme <- poc.theme + theme(plot.margin = unit(c(1, 1, 1, 1) * 5, "mm"))
    if (!gridlines) poc.theme <- poc.theme + theme(panel.grid = element_blank())
    return(poc.theme)
}
