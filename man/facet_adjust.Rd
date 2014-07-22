\name{facet_adjust}
\alias{facet_adjust}
\title{Use in place of \code{print.ggplot} with a jagged \code{facet_wrap}
to label all the horizontal axes.}
\usage{
  facet_adjust(x, pos = c("up", "down"),
    newpage = is.null(vp), vp = NULL)
}
\arguments{
  \item{x}{ggplot object to print}

  \item{pos}{position of the added axes. \code{"up"} for
  directly underneath, \code{"down"} for all in one line
  across the bottom}

  \item{newpage}{draw new (empty) page first?}

  \item{vp}{viewport to draw plot in}
}
\description{
  Use in place of \code{print.ggplot} with a jagged
  \code{facet_wrap} to label all the horizontal axes.
}

