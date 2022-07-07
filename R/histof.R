#' test for ABO compatibility
#'
#' @description ABO compatibility test between donor and candidate
#' @param cABO A character from (`env$valid.blood.groups`)
#' @param dABO A character from (`env$valid.blood.groups`)
#' @param iso A logical value T/F
#' @return A logical value T/F
#' @examples
#' abo(cABO = 'A', dABO = 'A', iso = TRUE)
#' @export
abo <- function(cABO = 'A', dABO = 'A', iso = TRUE){
  blood_group_checker(cABO) 
  blood_group_checker(dABO)
  is.logical(iso)

  if(iso == TRUE){
    value <- cABO == dABO
    } 
  else {
    value <- ifelse(dABO == 'O', TRUE,
              ifelse(dABO == 'A' & cABO %in% c('A','AB'),TRUE,
                      ifelse(dABO == 'B' & cABO %in% c('B','AB'),TRUE,
                            ifelse(dABO == 'AB' & cABO == 'AB', TRUE, FALSE)
                            )
                      )
              )
        }
  return(value)
    
}

#' number of HLA mismatchs
#'
#' @description Computes the number of HLA mismatchs between donor and candidate
#' @param dA donor's HLA-A typing
#' @param dB donor's HLA-B typing
#' @param dDR donor's HLA-DR typing
#' @param cA candidate's HLA-A typing
#' @param cB candidate's HLA-B typing
#' @param cDR candidate's HLA-DR typing
#' @return mmA number of HLA-A mismatchs between \code{dA} and \code{cA};
#' mmB number of HLA-B mismatchs between \code{dB} and \code{cB};
#' mmDR number of HLA-DR mismatchs between \code{dA}DRand \code{cDR};
#' and mmHLA as the sum of mmA + mmB + mmDR
#' @examples
#' mmHLA(dA = c('1','2'), dB = c('5','7'), dDR = c('1','4'),
#' cA = c('1','2'), cB = c('03','15'), cDR = c('04','07'))
#' @export
mmHLA <- function(dA = c('1','2'), 
                  dB = c('5','7'), 
                  dDR = c('1','4'),
                  cA = c('1','2'), 
                  cB = c('3','15'), 
                  cDR = c('4','7')){

  if(!is.character(dA)){stop("donor's HLA-A typing is not valid!\n")}
  if(!is.character(dB)){stop("donor's HLA-B typing is not valid!\n")}
  if(!is.character(dDR)){stop("donor's HLA-DR typing is not valid!\n")}
  if(!is.character(cA)){stop("candidate's HLA-A typing is not valid!\n")}
  if(!is.character(cB)){stop("candidate's HLA-B typing is not valid!\n")}
  if(!is.character(cDR)){stop("candidate's HLA-DR typing is not valid!\n")}
  
  mmA <- NULL
  mmB <- NULL
  mmDR <- NULL

  # compute missmatches
  mmA <- dplyr::if_else((dA[1] %in% cA & dA[2] %in% cA) | (dA[1] %in% cA & (is.na(dA[2]) | dA[2] == "")), 0,
                      dplyr::if_else(dA[1] %in% cA | dA[2] %in% cA, 1,
                                     dplyr::if_else(!dA[1] %in% cA & (is.na(dA[2]) | dA[2] == ""), 1,
                                                    dplyr::if_else(dA[1] == dA[2], 1,2))))

  mmB <- dplyr::if_else((dB[1] %in% cB & dB[2] %in% cB) | (dB[1] %in% cB & (is.na(dB[2]) | dB[2] == "")), 0,
                      dplyr::if_else(dB[1] %in% cB | dB[2] %in% cB, 1,
                                     dplyr::if_else(!dB[1] %in% cB & (is.na(dB[2]) | dB[2] == ""), 1,
                                                    dplyr::if_else(dB[1] == dB[2], 1,2))))

  mmDR <- dplyr::if_else((dDR[1] %in% cDR & dDR[2] %in% cDR) | (dDR[1] %in% cDR & (is.na(dDR[2]) | dDR[2] == "")), 0,
                       dplyr::if_else(dDR[1] %in% cDR | dDR[2] %in% cDR, 1,
                                      dplyr::if_else(!dDR[1] %in% cDR & (is.na(dDR[2]) | dDR[2] == ""), 1,
                                                     dplyr::if_else(dDR[1] == dDR[2],1,2))))

  # resume results
  mmHLA <- mmA + mmB + mmDR
  mm <- c(mmA,mmB,mmDR,mmHLA)
  names(mm) <- c("mmA","mmB","mmDR","mmHLA")

  return(mm)
}

#' virtual crossmatch (XM)
#'
#' @description returns candidates' virtual crossmatch againts donor's HLA typing
#' @param dA donor's HLA-A typing
#' @param dB donor's HLA-B typing
#' @param dDR donor's HLA-DR typing
#' @param df.abs data frame with candidates' antibodies
#' @param check.validity Logical to decide whether to validate input.
#' @return A dataframe with candidates' ID and xm result POS/NEG
#' @examples
#' xmatch(dA = c('1','2'), dB = c('5','7'), dDR = c('1','4'), df.abs = cabs, check.validity = TRUE)
#' @export
xmatch <- function(dA = c('1','2'),
                   dB = c('5','7'),
                   dDR = c('1','4'),
                   df.abs = cabs, 
                   check.validity = TRUE){
  if(check.validity){
    if(!requireNamespace("dplyr", quietly = TRUE)) {
      stop(
        "Package \"dplyr\" must be installed to use this function.",
        call. = FALSE
      )
    }
  }

  elements.are.chars <- function(array){
    for (element in array){
      if(!is.character(element)){
        stop("xmatch got a non-character HLA typing.")
      }
    }
  }

  elements.are.chars(dA)
  elements.are.chars(dB)
  elements.are.chars(dDR)

  dhla <- c(paste0('A',dA[1]),
            paste0('A',dA[2]),
            paste0('B',dB[1]),
            paste0('B',dB[2]),
            paste0('DR',dDR[1]),
            paste0('DR',dDR[2]))

  data.table::setDT(df.abs)

  df.abs[, res := abs %in% dhla]
  res <- df.abs[,
                xm := ifelse(sum(res)>0, "POS","NEG"),
                by = 'ID'][, .(xm[.N]),
                           by = 'ID']

  data.table::setnames(res, 'V1', 'xm')

  return(res[])
}

#' Hiperimunized classification
#'
#' @description returns candidates' hiperimunized classification according to a
#' cutoff value
#' @param cPRA candidate's cPRA value
#' @param cutoff A value to compare candidate's cPRA
#' @return A logical value T/F when cPRA >= cutoff
#' @examples
#' hiper(cPRA = 99, cutoff = 85)
#' @export
hiper <- function(cPRA = 99, cutoff = 85){
  cPRA_checker(cPRA)

  is.hiperimunized <- cPRA >= cutoff
  return(is.hiperimunized)
}

#' Senior Program classification
#'
#' @description Returns 1 when candidates' belongs to Senior Program.
#' Prioritization for older patients for older donors, followed for young patients
#' for younger donors, and for last the remaining patients.
#' @param donor.age A numeric value with donor's age
#' @param candidate.age A numeric value with candidate's age
#' @return The value 1 for a candidates older than 65 with also a donor
#' older than 65
#' @examples
#' sp(donor.age = 66, candidate.age = 70)
#' @export
sp <- function(donor.age, candidate.age){
  value <- ifelse(donor.age >= 65 & candidate.age >= 65, 1,
                  ifelse(donor.age < 65 & candidate.age < 65, 2,3))
  return(value)
}

#' TRANSPLANTSCORE (Tx Score)
#'
#' @description Returns the estimated 5-year event (mortality or graft failure
#' combined outcome) probability as described by Molnar, el al (2017).
#' @param recipient.age A numeric value with recipient's age
#' @param recipient.race A character value with recipient's race from the options:
#' 'White', 'Black', 'Hispanic', 'Other'
#' @param recipient.causeESRD A numeric value with recipient's cause of End-Stage Renal
#' Disease, with options: 'Other', 'Diabetes', 'Hypertension',
#' 'Glomerulonephritis', 'Cystic disease'
#' @param recipient.dialysis A numeric value with recipient's time on dialysis (months)
#' @param recipient.diabetes A logical value with recipient's diabetic status
#' @param recipient.coronary A logical value with recipient's coronary artery disease status
#' @param recipient.albumin A numeric value with recipient's albumin (g/dL)
#' @param recipient.hemoglobin A numeric value with recipient's hemoglobin (g/dL)
#' @param donor.age A numeric value with donor's age
#' @param donor.diabetes A logical value with donor's diabetic status, with options:
#' 'Absence', 'Presence', 'Unknown'
#' @param donor.ECD A logical value regarding Extended Criteria Donor
#' @param mmHLA_A A numeric value (0, 1, 2) with the number of HLA-A mismatchs
#' @param mmHLA_B A numeric value (0, 1, 2) with the number of HLA-B mismatchs
#' @param mmHLA_DR A numeric value (0, 1, 2) with the number of HLA-DR mismatchs
#' @return 5 year probability for combined outcome of mortality or graft failure
#' @examples
#' txscore(recipient.age = 20,
#' recipient.race = "White", #insurance = 0,
#' recipient.causeESRD = "Other",
#' recipient.dialysis = 12, recipient.diabetes = FALSE,
#' recipient.coronary = FALSE, recipient.albumin = 1.5,
#' recipient.hemoglobin = 10, donor.age = 30,
#' donor.diabetes = "Absence",
#' donor.ECD = FALSE, #mmHLA = "0",
#' mmHLA_A = 0, mmHLA_B = 0, mmHLA_DR = 0)
#' @source \url{https://balima.shinyapps.io/scoreTx/}
#' @export
txscore <- function(recipient.age = 20
                    , recipient.race = "White"
                    #, insurance = 0
                    , recipient.causeESRD = "Other"
                    , recipient.dialysis = 12
                    , recipient.diabetes = FALSE
                    , recipient.coronary = FALSE
                    , recipient.albumin = 1.5
                    , recipient.hemoglobin = 10
                    , donor.age = 30
                    , donor.diabetes = "Absence"
                    , donor.ECD = FALSE
                    #, mmHLA = "0"
                    , mmHLA_A = 0
                    , mmHLA_B = 0
                    , mmHLA_DR = 0){
  age_checker(recipient.age)
  if(!recipient.race %in% c('White','Black','Hispanic','Other')){stop("Recipient's race is not valid! Valid options: 'White','Black','Hispanic','Other'")}
  if(!recipient.causeESRD %in% c('Diabetes','Hypertension','Glomerulonephritis','Cystic Disease','Other')){stop("Recipient's cause of ESRD is not valid! Valid options: 'Diabetes','Hypertension','Glomerulonephritis','Cystic Disease','Other'")}
  if(!is.numeric(recipient.dialysis) | recipient.dialysis < 0 | recipient.dialysis > 200){stop("Recipient's Time on dialysis is not valid! Expected a value between 0 and 200")}
  if(!is.logical(recipient.diabetes)){stop("Recipient's diabetes is not valid! Expected a logical value.")}
  if(!is.logical(recipient.coronary)){stop("Recipient's coronary disease is not valid! Expected a logical value.")}
  if(!is.numeric(recipient.albumin) | recipient.albumin < 1 | recipient.albumin > 5){stop("Recipient's Albumin is not valid! Expected a value between 1 and 5")}
  if(!is.numeric(recipient.hemoglobin) | recipient.hemoglobin < 3 | recipient.hemoglobin > 20){stop("Recipient's Hemoglobin is not valid! Expected a value between 3 and 20")}
  age_checker(donor.age)
  if(!donor.diabetes %in% c('Absence','Presence','Unknown')){stop("Donor's diabetes is not valid! Valid options: 'Absence','Presence','Unknown'")}
  if(!is.logical(donor.ECD)){stop("Recipient's ECD is not valid! Expected a logical value.")}
  if(!mmHLA_A %in% c(0,1,2)){stop("Number of mm HLA-A is not valid! Valid optios: 0, 1, 2")}
  if(!mmHLA_B %in% c(0,1,2)){stop("Number of mm HLA-B is not valid! Valid optios: 0, 1, 2")}
  if(!mmHLA_DR %in% c(0,1,2)){stop("Number of mm HLA-DR is not valid! Valid optios: 0, 1, 2")}
  
  mmHLA_ <- as.numeric(mmHLA_A) + as.numeric(mmHLA_B) + as.numeric(mmHLA_DR)
  mmHLA <- ifelse(mmHLA_ == 0 , '0',
                  ifelse(mmHLA_ < 4, '1-3', '4-6'))

  recipient.age <- ifelse(recipient.age < 35 , 0.0993,
                 ifelse(recipient.age < 50 , -0.0784,
                        ifelse(recipient.age < 65, 0, 0.1881)))
  recipient.race <- ifelse(recipient.race == "White", 0,
                 ifelse(recipient.race == "Black", 0.1609,
                        ifelse(recipient.race == "Hispanic", -0.2554, -0.4475)))
  recipient.causeESRD <- ifelse(recipient.causeESRD == "Diabetes", 0,
                      ifelse(recipient.causeESRD == "Hypertension", 0.1541,
                             ifelse(recipient.causeESRD == "Glomerulonephritis", 0.1447,
                                    ifelse(recipient.causeESRD == "Cystic Disease", -0.1870, 0.3209))))
  recipient.dialysis <- ifelse(recipient.dialysis < 12, 0,
                  ifelse(recipient.dialysis < 36, -0.2618,
                         ifelse(recipient.dialysis < 61, -0.3747, -0.1432)))
  recipient.diabetes <- ifelse(recipient.diabetes == TRUE, 0.3021, 0)
  recipient.coronary <- ifelse(recipient.coronary == TRUE, 0.2617, 0)
  recipient.albumin <- (recipient.albumin - 4)*(-0.2644)
  recipient.hemoglobin <- (recipient.hemoglobin - 12.3)*(-0.0451)
  donor.age <- (donor.age - 39)*0.0059
  donor.diabetes <- ifelse(donor.diabetes == "Absence", 0,
                      ifelse(donor.diabetes == "Presence", 0.4596, -0.3308))
  donor.ECD <- ifelse(donor.ECD == TRUE, 0.2082, 0)
  mmHLA <- ifelse(mmHLA == "0" , 0,
                  ifelse(mmHLA == "1-3", 0.3241, 0.3115))

  LP <- recipient.age + recipient.race + recipient.causeESRD + recipient.dialysis +
    recipient.diabetes + recipient.coronary + recipient.albumin +
    recipient.hemoglobin + donor.age + donor.diabetes + donor.ECD +
    mmHLA

  gamma <- 0.916

  PS <- gamma * LP

  prob5y <- round((1-0.752292^exp(PS))*100,2)

  list(LP = LP
       , gamma = gamma
       , PS = PS
       , prob5y = prob5y)

}
