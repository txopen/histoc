% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/et.R
\name{et_mmp}
\alias{et_mmp}
\title{Mismatch Probability (MMP) from ETKAS}
\source{
\url{https://www.eurotransplant.org/allocation/eurotransplant-manual/}
}
\usage{
et_mmp(
  data = candidates,
  hlaA = hlaApt,
  hlaB = hlaBpt,
  hlaDR = hlaDRpt,
  abo.freq = ABOpt,
  check.validity = TRUE
)
}
\arguments{
\item{data}{A data frame containing demographics and medical information for a
group of waitlisted transplant candidates.}

\item{hlaA}{A data frame with HLA-A allele frequencies.}

\item{hlaB}{A data frame with HLA-B allele frequencies.}

\item{hlaDR}{A data frame with HLA-DR allele frequencies.}

\item{abo.freq}{A data frame with ABO blood group frequencies.}

\item{check.validity}{Logical to decide whether to validate input.}
}
\value{
A data table with columns for HLA allele frequencies and Mismatch Probability \code{MMP} (color priority),
}
\description{
Mismatch Probability (MMP) is a calculation of the probability of
receiving a kidney offer with
0 and 1 broad HLA-A, -B or DR mismatches based on 1000 kidneys offered,
taking into account ABO blood group rules and PRA screening. Patients receive
between 0-100 MMPs
}
\examples{
et_mmp(data = candidates,
hlaA = hlaApt, hlaB = hlaBpt, hlaDR = hlaDRpt,
abo.freq = ABOpt, check.validity = TRUE)
}
