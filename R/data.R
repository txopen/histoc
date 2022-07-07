#' Transplant candidates' HLA antibodies.
#'
#' A dataset containing HLA antibodies for a group of transplant candidates.
#'
#' @format A data frame with 752 rows and 2 variables:
#' \describe{
#'   \item{ID}{Candidates' identifier}
#'   \item{abs}{Candidates HLA antibodies for which they are himunezed to}
#'   ...
#' }
#' @source \url{https://balima.shinyapps.io/kars/}
"cabs"

#' Waiting list with transplant candidates' information.
#'
#' A dataset containing demographics and medical information for a group of transplant candidates.
#'
#' @format A data frame with 500 rows and 12 variables:
#' \describe{
#'   \item{ID}{Candidates' identifier}
#'   \item{bg}{Candidates' blood group}
#'   \item{A1}{HLA-A allele 1}
#'   \item{A2}{HLA-A allele 2}
#'   \item{B1}{HLA-B allele 1}
#'   \item{B2}{HLA-B allele 2}
#'   \item{DR1}{HLA-DR allele 1}
#'   \item{DR2}{HLA-DR allele 2}
#'   \item{age}{Candidates' age in years}
#'   \item{dialysis}{Candidates' time on dialysis in months}
#'   \item{cPRA}{Candidates' cPRA percentage}
#'   \item{urgent}{clinical urgency 0/1}
#'   ...
#' }
#' @source \url{https://balima.shinyapps.io/kars/}
"candidates"

#' Waiting list with UK transplant candidates' information.
#'
#' A dataset containing demographics and medical information (including factors used in UK transplant) for a group of transplant candidates.
#'
#' @format A data frame with 500 rows and 15 variables:
#' \describe{
#'   \item{ID}{Candidates' identifier}
#'   \item{bg}{Candidates' blood group}
#'   \item{A1}{HLA-A allele 1}
#'   \item{A2}{HLA-A allele 2}
#'   \item{B1}{HLA-B allele 1}
#'   \item{B2}{HLA-B allele 2}
#'   \item{DR1}{HLA-DR allele 1}
#'   \item{DR2}{HLA-DR allele 2}
#'   \item{age}{Candidates' age in years}
#'   \item{dialysis}{Candidates' time on dialysis in months}
#'   \item{cPRA}{Candidates' cPRA percentage}
#'   \item{Tier}{Candidates are ranked on two Tiers (A and B)}
#'   \item{MS}{Candidates' MAtch Score defined as HLA transplantability}
#'   \item{RRI}{A recipient risk score (RRI) is calculated, for each eligible patient using 4 risk factors: age, dialysis at registration, time on dialysis, diabetic status}
#'   \item{urgent}{clinical urgency 0/1}
#'   ...
#' }
#' @source \url{https://nhsbtdbe.blob.core.windows.net/umbraco-assets-corp/16915/kidney-allocation-policy-pol186.pdf}
"candidates.uk"

#'  Kidney donors' information.
#'
#' A dataset containing demographics and medical information for a pool of donors.
#'
#' @format A data frame with 70 rows and 9 variables:
#' \describe{
#'   \item{ID}{Donors' identifier}
#'   \item{bg}{Donors' blood group}
#'   \item{A1}{HLA-A allele 1}
#'   \item{A2}{HLA-A allele 2}
#'   \item{B1}{HLA-B allele 1}
#'   \item{B2}{HLA-B allele 2}
#'   \item{DR1}{HLA-DR allele 1}
#'   \item{DR2}{HLA-DR allele 2}
#'   \item{age}{Donors' age in years}
#'   ...
#' }
#' @source \url{https://balima.shinyapps.io/kars/}
"donors"

#'  Kidney donors' information from UK transplant.
#'
#' A dataset containing demographics and medical information (including factors used in UK transplant) for a pool of donors.
#'
#' @format A data frame with 70 rows and 10 variables:
#' \describe{
#'   \item{ID}{Donors' identifier}
#'   \item{bg}{Donors' blood group}
#'   \item{A1}{HLA-A allele 1}
#'   \item{A2}{HLA-A allele 2}
#'   \item{B1}{HLA-B allele 1}
#'   \item{B2}{HLA-B allele 2}
#'   \item{DR1}{HLA-DR allele 1}
#'   \item{DR2}{HLA-DR allele 2}
#'   \item{age}{Donors' age in years}
#'   \item{DRI}{A donor risk score (DRI) is calculated for each donor on offer using 7 risk factors: age, history of hypertension; gender; CMV+, eGFR, days in hospital}
#'   ...
#' }
#' @source \url{https://nhsbtdbe.blob.core.windows.net/umbraco-assets-corp/16915/kidney-allocation-policy-pol186.pdf}
"donors.uk"

#' HLA-A relative frequencies.
#'
#' A dataset containing HLA-A allele relative frequencies from portuguese donors used for MMP computation.
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{A}{HLA-A allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://12f11c1f-960a-f627-594d-b8ce276384f7.filesusr.com/ugd/3e838e_dc548dede99a4db5869c3d2c20c2d16f.pdf?index=true}
"hlaApt"

#' HLA-B relative frequencies.
#'
#' A dataset containing HLA-B allele relative frequencies from portuguese donors used for MMP computation.
#'
#' @format A data frame with 34 rows and 3 variables:
#' \describe{
#'   \item{B}{HLA-B allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://12f11c1f-960a-f627-594d-b8ce276384f7.filesusr.com/ugd/3e838e_dc548dede99a4db5869c3d2c20c2d16f.pdf?index=true}
"hlaBpt"

#' HLA-DR relative frequencies.
#'
#' A dataset containing HLA-DR allele relative frequencies from portuguese donors used for MMP computation.
#'
#' @format A data frame with 13 rows and 3 variables:
#' \describe{
#'   \item{DR}{HLA-DR allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://12f11c1f-960a-f627-594d-b8ce276384f7.filesusr.com/ugd/3e838e_dc548dede99a4db5869c3d2c20c2d16f.pdf?index=true}
"hlaDRpt"

#' ABO relative frequencies.
#'
#' A dataset containing ABO relative frequencies from portuguese blood donors used for MMP computation.
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{abo}{ABO froup}
#'   \item{freq}{relative frequencies}
#'   ...
#' }
#' @source \url{http://www.ipst.pt/files/IPST/INFORMACAO_DOCUMENTACAO/AB0_29_2007_pag_5a17.pdf}
"ABOpt"

#' HLA-A relative frequencies.
#'
#' A dataset containing HLA-A allele relative frequencies from EuroTransplant donors used for MMP computation.
#'
#' @format A data frame with 27 rows and 3 variables:
#' \describe{
#'   \item{A}{HLA-A allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://www.eurotransplant.org/wp-content/uploads/2020/01/H4-Kidney.pdf}
"hlaAet"

#' HLA-B relative frequencies.
#'
#' A dataset containing HLA-B allele relative frequencies from EuroTransplant donors used for MMP computation.
#'
#' @format A data frame with 57 rows and 3 variables:
#' \describe{
#'   \item{B}{HLA-B allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://www.eurotransplant.org/wp-content/uploads/2020/01/H4-Kidney.pdf}
"hlaBet"

#' HLA-DR relative frequencies.
#'
#' A dataset containing HLA-DR allele relative frequencies from EuroTransplant donors used for MMP computation.
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{DR}{HLA-DR allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://www.eurotransplant.org/wp-content/uploads/2020/01/H4-Kidney.pdf}
"hlaDRet"
