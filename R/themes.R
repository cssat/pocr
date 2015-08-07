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
#' @param gridlines boolean for whether there should be any gridlines (TRUE)
#' @param expand.margin boolean for whether the right margin needs padding. Useful if \code{slant}
#' is \code{TRUE} and the labels on the right side are lengthy.
#' @param ... additional arguments passed to theme_bw
#' 
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
    if (slant) poc.theme <- 
        poc.theme + theme(axis.text.x = element_text(angle = -25, hjust = 0))
    if (expand.margin) poc.theme <- 
        poc.theme + theme(plot.margin = unit(c(1, 1, 1, 1) * 5, "mm"))
    if (!gridlines) poc.theme <- 
        poc.theme + theme(panel.grid = element_blank())
    return(poc.theme)
}