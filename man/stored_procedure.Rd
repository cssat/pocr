\name{stored_procedure}
\alias{stored_procedure}
\title{Produces strings for stored procedure calls}
\usage{
  stored_procedure(sp = c("poc1ab", "poc2", "poc3", "discharge_exit_over_time", "discharge_exit_over_time_mr", "PBCP5", "PBCS2", "PBCS3", "PBCW3", "PBCW4"),
    db = c("mysql", "sqlserver", "poc", "test_annie"),
    type = c("", "counts", "perCapita", "MR", "kin"),
    date = "2008-01-01", age = 0, race = 0, gender = 0,
    init = 0, last = 0, reason = 0, county = 0,
    custody = 0, office = 0, county_grp = NA, siblings = 0)
}
\arguments{
  \item{sp}{(matches) Name of stored procedure (sans any
  "sp_" prefix or suffix such as "counts").}

  \item{db}{(matches) Database to use. Defaults to "mysql"
  (equivalent to "test_annie"), the other option is
  "sqlserver" (equivalent to "poc").}

  \item{type}{(matches) Suffix of stored procedure (e.g.
  "counts", "perCapita", "MR", or "kin").}

  \item{date}{Point-in-time or cohort entry/exit date(s)
  here. (See also \code{\link{seq.Date}}.)}

  \item{age}{age/age group code, see details.}

  \item{race}{race/ethnicity, see details.}

  \item{gender}{1 for Female, 2 for Male (0 for all).}

  \item{init}{Initial placement setting, see details.}

  \item{last}{Last placement setting, see details.}

  \item{reason}{Removal reason.}

  \item{county}{County code.}

  \item{custody}{Custody removed from.}

  \item{office}{Office code.}

  \item{county_grp}{County Group Code.}

  \item{siblings}{Sibling group code 0: All, 1: 2 siblings,
  2: 3 siblings, 3: 4 siblings, 4: >4 siblings.}
}
\description{
  Every argument defaults to "0" for all. (Except date
  which defaults to "2008-01-01".) Arguments will be
  coerced to character, so plain sequences are fine.
}
\details{
  Arguments described above as "matches" will be matched if
  a partial arg is given. For example, setting parameter
  \code{sp = "dis"} will be enough to find the
  discharge_exit_over_time stored procedure.

  Vector arguments will be handled appropriately, so if you
  want to make a query for all 39 counties just use
  \code{county = 1:39}. Character counties will be
  converted to numerics, so specifying \code{county =
  "Kitsap"} or \code{county = c("king, pierce")} will also
  work (capitalization is ignored).

  On TO-DO list: make it smarter about recognizing text
  office and county groups.
}
\examples{
\dontrun{storedProcedure("poc1ab", date = seq(as.Date("2006-01-01"),
    as.Date("2009-01-01"), by = "year"), age = 1:3)}
}

