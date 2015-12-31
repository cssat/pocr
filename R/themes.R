###############################################################################
## themes.R

# This is a collection of ggplot themes identified as particularly useful for
# POC data work.

###############################################################################

#' A blank ggplot theme, borrowed from somebody on github
#' 
#' @param base_size defaults to 12, should be fine mostly
#' @param ... additional arguments based to theme_gray
#' 
#' @export
theme_clean <- function(base_size=12, ...) {
    theme_grey(base_size, ...) %+replace%
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

#' POC theme for ggplot
#' 
#' Based off of \code{theme_bw}, this frames the plot in POC blue,
#' eliminated minor grid lines, and will try to match to a POC font.
#' There are also some other options.
#' 
#' @param font Partial-matches to "Arial", "PT Sans" (for web) or "Frutiger LT Std 45 Light" (for print).
#' @param slant boolean for whether x-axis labels should be slanted. (FALSE)
#' @param gridlines What gridlines to include. Deafult is \code{"xy"}. Use \code{"x"} or \code{"y"}
#' for x or y only, any of \code{""}, \code{NA}, or \code{"none"} will result in no lines.
#' @param expand.margin boolean for whether the right margin needs padding. Useful if \code{slant}
#' is \code{TRUE} and the labels on the right side are lengthy.
#' @param ... additional arguments passed to theme_bw
#' 
#' @export
theme_poc <- function(
    font = "Open Sans",
    slant = FALSE,
    gridlines = c("xy", "x", "y", "", "none", NA),
    expand.margin = FALSE,
    ...) {
    if (font %nin% fonts()) {
        warning("Looks like you don't have ", font, " installed. Attempting to use Arial.")
        font <- "Arial"
    }

    if (is.logical(gridlines)) { 
        # for backwards compatibility
        gridlines = ifelse(gridlines, "xy", "none")
        warning("Use of a logical grid lines argument is no longer recommended.")
    }
    gridlines = match.arg(gridlines)
    if (gridlines %in% c("", "none") | is.na(gridlines)) gridlines = "none"
    
    poc.theme <- theme_bw(..., base_family = font) +
        theme(panel.border = element_rect(size = 1, colour = poc_colors[1]),
              axis.ticks = element_line(size = 1, colour = poc_colors[1]),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = poc_colors[1]),
              strip.text.x = element_text(colour = "white"),
              strip.text.x = element_text(colour = "white"))
    if (slant) poc.theme <- 
        poc.theme + theme(axis.text.x = element_text(angle = -25, hjust = 0))
    if (expand.margin) poc.theme <- 
        poc.theme + theme(plot.margin = unit(c(1, 1, 1, 1) * 5, "mm"))
    poc.theme = poc.theme + switch(
        gridlines,
        xy = theme(),
        x = theme(panel.grid.y = element_blank()),
        y = theme(panel.grid.x = element_blank()),
        none = theme(panel.grid = element_blank())
    )                           
    return(poc.theme)
}