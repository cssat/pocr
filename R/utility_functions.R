#' Shorthand for \code{summary()}
#' @export
su <- summary

#' Shorthand for \code{head()}
#' @export
h <- head

#' Sample of an object.
#' 
#' Shortcut for checking on data.
#' @rdname samp
#' @param x data.frame (or matrix) to sample from
#' @param n number of rows to sample (default is 10)
#' @return n random rows of x
#' @export
samp <- function(x, n) {
    UseMethod("samp")
}

#' @rdname samp
#' @method samp data.frame
#' @S3method samp data.frame
samp.data.frame <- function(x, n = 10) {
    if (n > nrow(x)) {
	    warning("samp: object smaller than sample size, whole object returned.")
	    return(x)
	}
    x[sample(nrow(x), size = n), ]
}

#' @rdname samp
#' @method samp matrix
#' @S3method samp matrix
samp.matrix <- function(x, n = 10) {
    if (n > nrow(x)) {
	    warning("samp: object smaller than sample size, whole object returned.")
	    return(x)
	}
    x[sample(nrow(x), size = n), ]
}

#' @rdname samp
#' @method samp default
#' @S3method samp default
samp.default <- function(x, n = 10) {
    if (n > length(x)) {
	    warning("samp: object smaller than sample size, whole object returned.")
	    return(x)
	}
    x[sample(length(x), size = n)]
}

#' A blank ggplot theme, borrowed from somebody on github
#' 
#' @param base_size defaults to 12, should be fine mostly
#' @export
theme_clean <- function(base_size=12) {
    require(grid) 
    theme_grey(base_size) %+replace%
        theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.length = unit(0,"cm"),
            axis.ticks.margin = unit(0.01,"cm"),
            panel.margin = unit(0,"lines"),
            plot.margin = unit(c(0,0,0,0),"lines"),
            complete = TRUE
        )    
}

#' Sets working directory to \code{"S:/Data Portal"}
#' @export
resetwd <- function() {
    setwd("S:/Data Portal")
}

#' Dummy function for kml help
#' 
#' Reading and Writing KML files:
#' \code{readOGR(dsn = "C:/Users/gregorp.NEBULA2/Downloads/1433", layer = "wa_counties", encoding="KML")}
#' Notice, for reading: no file extension, layer taken from inside the \code{<name>} tags in the KML file,
#' encoding specified.
#' \code{writeOGR(obj = obName, dsn = "directory/file.kml", driver = "KML", layer = "layerName")}
#' Writing KML is basically the same.
#' 
#' Reading SHP files
#' \code{readOGR(dsn = "C:/Users/gregorp.NEBULA2/Downloads/1433", layer = "wa_counties")}
#' \code{dsn} points to directory, layer is name of particular shape file (sans file extension).
#' @export
kml <- function() {
    print("See ?kml for instructions on reading and writing kml files.")
    NULL
}


#' Takes a vector and returns a comma-space separated string for printing.
#' 
#' @param x Vector to paste together
#' @param and Boolean, whether an "and" should be inserted before the last element 
#' @param oxford Boolean, whether to include an "Oxford comma" (before the "and")
#' @export
comma_vector <- function(x, and = TRUE, oxford = TRUE) {
    n <- length(x)
    if (n == 1) return (x)
    if (and) {
        if (n == 2) return (paste(x[1], "and", x[2]))
        return (paste0(paste(x[1:(n-1)], collapse = ", "), ifelse(oxford, ", ", " "), "and ", x[n]))
    }
    return (paste(x, collapse = ", "))
}


#' Outputs a nice date for inline inclusion (wrapper for strftime)
#' 
#' @param x vector of POSIX objects (or something that can be coerced that way)
#' @param format  character with "m", "d", and "y", in the order you want them.
#' @param abbreviate boolean, whether to abbreviate the day.
#' @export
pretty_date <- function(x,
                       format = c("mdy", "dmy", "ymd"),
                       abbreviate = FALSE, ...) {
    format <- match.arg(format)
    if (format == "mdy" & !abbreviate) return(strftime(x, format = "%B %d, %Y", ...))
    if (format == "mdy" &  abbreviate) return(strftime(x, format = "%b %d, %Y", ...))
    if (format == "dmy" & !abbreviate) return(strftime(x, format = "%d %B %Y", ...))
    if (format == "dmy" &  abbreviate) return(strftime(x, format = "%d %b %Y", ...))
    if (format == "ymd" & !abbreviate) return(strftime(x, format = "%Y %B %d", ...))
    if (format == "ymd" &  abbreviate) return(strftime(x, format = "%Y %b %d", ...))  
}


#' @export
`%nin%` <- function (x, table) match(x, table, nomatch = 0) == 0


#' Use in place of \code{print.ggplot} with a jagged \code{facet_wrap}
#' to label all the horizontal axes.
#' 
#' @param x ggplot object to print
#' @param pos position of the added axes. \code{"up"} for directly underneath,
#' \code{"down"} for all in one line across the bottom
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @export
facet_adjust <- function(x, pos = c("up", "down"), 
                         newpage = is.null(vp), vp = NULL)
{
    # part of print.ggplot
    ggplot2:::set_last_plot(x)
    if(newpage)
        grid.newpage()
    pos <- match.arg(pos)
    p <- ggplot_build(x)
    gtable <- ggplot_gtable(p)
    # finding dimensions
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    # number of panels in the plot
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    # missing panels
    n <- space - panels
    # checking whether modifications are needed
    if(panels != space){
        # indices of panels to fix
        idx <- (space - ncol - n + 1):(space - ncol)
        # copying x-axis of the last existing panel to the chosen panels 
        # in the row above
        gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
        if(pos == "down"){
            # if pos == down then shifting labels down to the same level as 
            # the x-axis of last panel
            rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                         gtable$layout$name)
            lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
            gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
        }
    }
    # again part of print.ggplot, plotting adjusted version
    if(is.null(vp)){
        grid.draw(gtable)
    }
    else{
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
        grid.draw(gtable)
        upViewport()
    }
    invisible(p)
}


#' Mode of a vector
#' 
#' Found on Stackoverflow.
#' 
#' @param x Vector for which you want to know the mode.
#' @export
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#' Export \code{data.frame} to \code{JSON}
#' 
#' This is based on the type of JSON file that the original nutrient Parallel Coordinates
#' was built off of. It seems to be sort of "row oriented", whereas \code{rjson::toJSON}
#' is "column oriented".
#' 
#' @param df Data frame to convert.
#' @param file name of file to save to.
#' @param varname name of variable to assign to the JSON object.
#' @export
df_to_json <- function(df, file, varname = "foods") {
    sink(file)
    cat("var", varname, "= [")
    classes <- sapply(df, class)
    rown <- nrow(df)
    coln <- ncol(df)
    dfnames <- names(df)
    for (rowi in 1:rown) {
        cat("{")
        for (colj in 1:coln) {
            cat('"', dfnames[colj], '":', sep = "")   ## "column name":
            if(classes[colj] != "numeric") cat('"') ## non-numeric values need to be quoted
            cat(df[rowi, colj])    ## value
            if(classes[colj] != "numeric") cat('"') ## non-numeric values need to be quoted
            if (colj < coln) cat(",")
        }
        cat("}")
        if (rowi < rown) cat(",")
    }
    cat("];")
    sink()
}


#' @name portal_colors
#' @title portal_colors
#' @description Vector of 12 hex colors used in the portal
#' @docType data
NULL

#' @name poc_colors
#' @title poc_colors
#' @description Vector of 4 hex colors from POC theme.
#' @docType data
NULL


