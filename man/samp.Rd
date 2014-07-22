\name{samp}
\alias{samp}
\alias{samp.data.frame}
\alias{samp.default}
\title{Sample of an object.}
\usage{
  samp(x, n)

  \method{samp}{data.frame} (x, n = 10)

  \method{samp}{default} (x, n = 10)
}
\arguments{
  \item{x}{data.frame (or matrix) to sample from}

  \item{n}{number of rows to sample (default is 10)}
}
\value{
  n random rows of x
}
\description{
  Shortcut for checking on data.
}

