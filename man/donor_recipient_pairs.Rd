% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mult.R
\name{donor_recipient_pairs}
\alias{donor_recipient_pairs}
\title{Candidates' selection according to any algorithm for multiple donors}
\usage{
donor_recipient_pairs(
  df.donors = donors,
  df.candidates = candidates,
  df.abs = cabs,
  algorithm = lima,
  n = 2,
  check.validity = TRUE,
  ...
)
}
\arguments{
\item{df.donors}{A data frame containing demographics and medical information
for a pool of donors. For \code{uk} algorithm must have their respective columns.}

\item{df.candidates}{A data frame containing demographics and medical information
for a group of waitlisted transplant candidates. For \code{uk} algorithm must have respective columns.}

\item{df.abs}{A data frame with candidates' antibodies.}

\item{algorithm}{The name of the function to use. Valid options are:
\code{lima}, \code{et}, \code{pts}, \code{uk} (without quotation)}

\item{n}{A positive integer to slice the first candidates.}

\item{check.validity}{Logical to decide whether to validate input arguments.}

\item{...}{all the parameters used on the algorithm function.}
}
\value{
A list with the number of elements equal to the number of rows on donors' data frame.
Each element have a data frame with selected candidates by donor.
}
\description{
Ordering of waitlisted candidates for a each donor in a pool of donors and
according to any algorithm.
}
\examples{
\donttest{
donor_recipient_pairs(df.donors = donors,
df.candidates = candidates,
df.abs = cabs,
algorithm = lima,
n = 2,
check.validity = TRUE)
}
}
