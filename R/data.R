#' Transplant candidates' HLA antibodies.
#'
#' A dataset containing HLA antibodies for a group of transplant candidates.
#'
#' @format A data frame with 752 rows and 2 variables:
#' \describe{
#'   \item{ID}{Candidates' identifier}
#'   \item{abs}{Candidates HLA antibodies for which they are himmunized to}
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
#'   \item{bg}{Candidates' blood group:   `r env$valid.blood.groups `}
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
#'   \item{bg}{Candidates' blood group:   `r env$valid.blood.groups `}
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
#'   \item{MS}{Candidates' Match Score defined as HLA transplantability}
#'   \item{RRI}{A recipient risk score (RRI) calculated for each eligible patient using 4 risk factors: age, dialysis at registration, time on dialysis, diabetic status}
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
#'   \item{bg}{Donors' blood group:   `r env$valid.blood.groups `}
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
#'   \item{bg}{Donors' blood group:   `r env$valid.blood.groups `}
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
#' A dataset containing HLA-A allele relative frequencies from Portuguese donors
#' used for Mismatch Probability (MMP) computation.
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
#' A dataset containing HLA-B allele relative frequencies from Portuguese donors
#' used for Mismatch Probability (MMP) computation.
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
#' A dataset containing HLA-DR allele relative frequencies from Portuguese donors
#' used for Mismatch Probability (MMP) computation.
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
#' A dataset containing ABO relative frequencies from Portuguese blood donors
#' used for Mismatch Probability (MMP) computation.
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{abo}{ABO group}
#'   \item{freq}{relative frequencies}
#'   ...
#' }
#' @source \url{http://www.ipst.pt/files/IPST/INFORMACAO_DOCUMENTACAO/AB0_29_2007_pag_5a17.pdf}
"ABOpt"

#' HLA-A relative frequencies.
#'
#' A dataset containing HLA-A allele relative frequencies from EuroTransplant donors
#' used for Mismatch Probability (MMP) computation.
#'
#' @format A data frame with 27 rows and 3 variables:
#' \describe{
#'   \item{A}{HLA-A allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://www.eurotransplant.org/allocation/eurotransplant-manual/}
"hlaAet"

#' HLA-B relative frequencies.
#'
#' A dataset containing HLA-B allele relative frequencies from EuroTransplant donors
#' used for Mismatch Probability (MMP) computation.
#'
#' @format A data frame with 57 rows and 3 variables:
#' \describe{
#'   \item{B}{HLA-B allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://www.eurotransplant.org/allocation/eurotransplant-manual/}
"hlaBet"

#' HLA-DR relative frequencies.
#'
#' A dataset containing HLA-DR allele relative frequencies from EuroTransplant donors
#' used for Mismatch Probability (MMP) computation.
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{DR}{HLA-DR allele}
#'   \item{n}{allele counts}
#'   \item{freq}{allele frequencies}
#'   ...
#' }
#' @source \url{https://www.eurotransplant.org/allocation/eurotransplant-manual/}
"hlaDRet"

#' HLA-A, -B, -C amino-acid sequences in FASTA format.
#'
#' The protein sequence of exons 2 and 3 of each allele of HLA classe I genotype;
#' these sequences correspond to the peptide-binding domains. Availabble from: https://www.ebi.ac.uk/ipd/imgt/hla/
#'
#' @format list in FASTA format representing either nucleotide sequences or amino acid (protein) sequences, in which nucleotides or amino acids are represented using single-letter codes:
#' \describe{
#'   \item{X*}{HLA-X allele}
#'   \item{name}{allele name}
#'   \item{Annot}{annotation}
#'   \item{class}{SeqFastadna}
#'   ...
#' }
#' @source \url{https://github.com/sunhuaibo/HLA-HED/blob/main/database/ABC_prot.fa}
"seqs"

#' HLA-DRB1, -DQB1 amino-acid sequences in FASTA format.
#'
#' The protein sequence of exon 2 of each allele of HLA classe II genotype;
#' these sequences correspond to the peptide-binding domains.
#'
#' @format list in FASTA format representing either nucleotide sequences or amino acid (protein) sequences, in which nucleotides or amino acids are represented using single-letter codes:
#' \describe{
#'   \item{X*}{HLA-X allele}
#'   \item{name}{allele name}
#'   \item{Annot}{annotation}
#'   \item{class}{SeqFastadna}
#'   ...
#' }
#' @source \url{https://sourceforge.net/projects/granthamdist/files/HLA_ClassII_CWDonly.fas/download}
"seqs2"

#' Grantham distance metric.
#'
#' The Grantham score attempts to predict the distance between two amino acids, in an evolutionary sense.
#' A lower Grantham score reflects less evolutionary distance.
#' A higher Grantham score reflects a greater evolutionary distance.
#'
#' @format a 20x20 matrix
#' \describe{
#'   \item{rows}{amino acid}
#'   \item{columns}{amino acid}
#' }
#' @source \url{https://www.science.org/doi/10.1126/science.185.4154.862}
"dst"
