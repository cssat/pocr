\name{fill_in}
\alias{fill_in}
\title{Expands a data.frame to include all unique combinations of variables
and fills in 0's (or other) for missing values}
\usage{
  fill_in(dat, value_name = NULL, value_index = NULL,
    fill_value = 0)
}
\arguments{
  \item{dat}{data.frame to fill in}

  \item{value_name}{names of columns to be filled in
  (specify this or \code{value_index})}

  \item{value_index}{indices of columns to be filled in
  (specify this or \code{value_name})}

  \item{fill_value}{how to fill in holes after expansion.
  (Defaults to 0.)}
}
\description{
  Expands a data.frame to include all unique combinations
  of variables and fills in 0's (or other) for missing
  values
}

