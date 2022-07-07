#' Mismatch Probability (MMP) from ETKAS
#'
#' @description Mismatch Probability (MMP) is a calculation of the probability of
#' receiving a kidney offer with
#' 0 and 1 broad HLA-A, -B or split DR mismatches based on 1000 kidneys offered,
#' taking into account AB0 blood group rules and PRA screening. Patients receive
#' between 0-100 MMPs
#' @source \url{https://www.eurotransplant.org/wp-content/uploads/2020/01/H4-Kidney.pdf}
#' @param data A data frame containing demographics and medical information for a
#' group of waitlisted transplant candidates.
#' @param hlaA A data frame with HLA-A allele frequencies
#' @param hlaB A data frame with HLA-B allele frequencies
#' @param hlaDR A data frame with HLA-DR allele frequencies
#' @param abo_freq A data frame with ABO blood group frequencies
#' @param check.validity Logical to decide whether to validate input.
#' @examples
#' et_mmp(data = candidates,
#' hlaA = hlaApt, hlaB = hlaBpt, hlaDR = hlaDRpt,
#' abo_freq = ABOpt, check.validity = TRUE)
#' @export
et_mmp <- function(data = candidates,
                 hlaA = hlaApt, 
                 hlaB = hlaBpt, 
                 hlaDR = hlaDRpt,
                 abo_freq = ABOpt,
                 check.validity = TRUE){

  if(check.validity){
    candidate_dataframe_check(data)
  }

  # compute the sum of squared frequencies for each loci with PT frequencies
  SallA <- sum((hlaA %>% tidyr::drop_na() %>% .$freq) ^ 2)
  SallB <- sum((hlaB %>% tidyr::drop_na() %>% .$freq) ^ 2)
  SallDR <- sum((hlaDR %>% tidyr::drop_na() %>% .$freq) ^ 2)

  data.table::setDT(data, key = 'ID')
  data.table::setDT(hlaA)
  data.table::setDT(hlaB)
  data.table::setDT(hlaDR)
  data.table::setDT(abo_freq)

  data <- hlaA[, .(A, freq)][data, on = .(A = A1)]
  setnames(data, "freq", "A1_freq")
  data <- hlaA[, .(A, freq)][data, on = .(A = A2)]
  setnames(data, "freq", "A2_freq")
  setnames(data, "i.A", "A1")
  setnames(data, "A", "A2")

  data <- hlaB[, .(B, freq)][data, on = .(B = B1)]
  setnames(data, "freq", "B1_freq")
  data <- hlaB[, .(B, freq)][data, on = .(B = B2)]
  setnames(data, "freq", "B2_freq")
  setnames(data, "i.B", "B1")
  setnames(data, "B", "B2")

  data <- hlaDR[, .(DR, freq)][data, on = .(DR = DR1)]
  setnames(data, "freq", "DR1_freq")
  data <- hlaDR[, .(DR, freq)][data, on = .(DR = DR2)]
  setnames(data, "freq", "DR2_freq")
  setnames(data, "i.DR", "DR1")
  setnames(data, "DR", "DR2")

  data <- abo_freq[data, on = .(abo = bg)]
  setnames(data, "abo", "bg")
  setnames(data, "freq", "abo")

  data[, `:=`(
    MMP0 = (A1_freq + A2_freq) ^ 2 * (B1_freq + B2_freq) ^ 2 * (DR1_freq + DR2_freq) ^ 2,
    MMP2 = (((2 * (A1_freq + A2_freq) * (1 - A1_freq - A2_freq)) - A1_freq ^ 2 - A2_freq ^ 2 + SallA) / ((A1_freq + A2_freq) ^ 2))
         + (((2 * (B1_freq + B2_freq) * (1 - B1_freq - B2_freq)) - B1_freq ^ 2 - B2_freq ^ 2 + SallB) / ((B1_freq + B2_freq) ^ 2))
         + (((2 * (DR1_freq + DR2_freq) * (1 - DR1_freq - DR2_freq)) - DR1_freq ^ 2 - DR2_freq ^ 2 + SallDR) / ((DR1_freq + DR2_freq) ^ 2))
    ),
    by = 'ID'
  ]

  data[, `:=`(MMP1 = MMP0 * MMP2), by = 'ID']

  data[, `:=`(
    MMP = 100 * ( 1 - (abo * (1 - cPRA / 100) * (MMP0 + MMP1))) ^ 1000
    ),
    by = 'ID'
  ]

  return(data[])
  }

#' ET points for mmHLA
#'
#' @description Computes HLA mismatches and the respective punctuation within ET
#' Kidney allocation system
#' @param dA donor's HLA-A typing
#' @param dB donor's HLA-B typing
#' @param dDR donor's HLA-DR typing
#' @param cA candidate's HLA-A typing
#' @param cB candidate's HLA-B typing
#' @param cDR candidate's HLA-DR typing
#' @param mm0 A numeric value with points for 0 HLA mm on ETKAS points table
#' @param mm1 A numeric value with points for 1 HLA mm on ETKAS points table
#' @param mm2 A numeric value with points for 2 HLA mm on ETKAS points table
#' @param mm3 A numeric value with points for 3 HLA mm on ETKAS points table
#' @param mm4 A numeric value with points for 4 HLA mm on ETKAS points table
#' @param mm5 A numeric value with points for 5 HLA mm on ETKAS points table
#' @param mm6 A numeric value with points for 6 HLA mm on ETKAS points table
#' @examples
#' et_mmHLA(dA = c("01","02"), dB = c("03","05"), dDR = c("04","06"),
#' cA = c("01","02"), cB = c("03","05"), cDR = c("04","06"),
#' mm0 = 400, mm1 = 333.33, mm2 = 266.67, mm3 = 200,
#' mm4 = 133.33, mm5 = 66.67, mm6 = 0)
#' @export
et_mmHLA <- function(dA = c("01","02"), dB = c("03","05"), dDR = c("04","06"),
                   cA = c("01","02"), cB = c("03","05"), cDR = c("04","06"),
                   mm0 = 400,
                   mm1 = 333.33,
                   mm2 = 266.67,
                   mm3 = 200,
                   mm4 = 133.33,
                   mm5 = 66.67,
                   mm6 = 0){
  if(!is.numeric(mm0) | mm0 < 0 | mm0 > 501){
    stop("points for 0 mmHLA (full match) is not valid!\n")}
  if(!is.numeric(mm1) | mm1 < 0 | mm1 > 501){
    stop("points for 1 mmHLA is not valid!\n")}
  if(!is.numeric(mm2) | mm2 < 0 | mm2 > 501){
    stop("points for 2 mmHLA is not valid!\n")}
  if(!is.numeric(mm3) | mm3 < 0 | mm3 > 501){
    stop("points for 3 mmHLA is not valid!\n")}
  if(!is.numeric(mm4) | mm4 < 0 | mm4 > 501){
    stop("points for 4 mmHLA is not valid!\n")}
  if(!is.numeric(mm5) | mm5 < 0 | mm5 > 501){
    stop("points for 5 mmHLA is not valid!\n")}
  if(!is.numeric(mm6) | mm6 < 0 | mm6 > 501){
    stop("points for 6 mmHLA is not valid!\n")}

  # apply mmHLA function
  mm <- mmHLA(dA = dA, dB = dB, dDR = dDR,
            cA = cA, cB = cB, cDR = cDR)

  pts <- switch(mm[[4]] +1,
                mm0, mm1, mm2, mm3, mm4, mm5, mm6)
  names(pts) <- 'ptsHLA'

  res <- c(mm,pts)

  return(res)
}


#' ET points for time on dialysis (in months)
#'
#' @description Punctuation given for each month on dialysis, within ET
#' Kidney allocation system
#' @param dialysis A numeric value with candidate's time on dialysis, in months
#' (between `env$dialysis.minimum` and `env$dialysis.maximum`)
#' @param month A numeric value with the punctuation for each month
#' (between env$month.points.minimum and env$month.points.maximum)
#' @examples
#' et_dialysis(dialysis = 100, month = 2.78)
#' @export
et_dialysis <- function(dialysis = 0, month = 2.78){
  dialysis_checker(dialysis)

  if(!is.numeric(month) | month < env$month.points.minimum | month > env$month.points.maximum){
    stop("Attributed points for each month on dialysis is not valid!\n")
  }

  pts <- dialysis * month

  return(pts)
}


#' resume function for ET algorithm punctuation
#'
#' @description Ordering of waitlisted candidates for a given donor and
#' according to ETKAS algorithm.
#' @param iso A logical value for isogroupal compatibility.
#' @param dABO A character value with ABO blood group (`env$valid.blood.groups`).
#' @param dA donor's HLA-A typing.
#' @param dB donor's HLA-B typing.
#' @param dDR donor's HLA-DR typing.
#' @param donor.age A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information for
#' a group of waitlisted transplant candidates.
#' @param month A numeric value with the punctuation for each month
#' (between env$month.points.minimum and env$month.points.maximum)
#' @param mm0 A numeric value with points for 0 HLA mm on ETKAS points table
#' @param mm1 A numeric value with points for 1 HLA mm on ETKAS points table
#' @param mm2 A numeric value with points for 2 HLA mm on ETKAS points table
#' @param mm3 A numeric value with points for 3 HLA mm on ETKAS points table
#' @param mm4 A numeric value with points for 4 HLA mm on ETKAS points table
#' @param mm5 A numeric value with points for 5 HLA mm on ETKAS points table
#' @param mm6 A numeric value with points for 6 HLA mm on ETKAS points table
#' @param hlaA A data frame with HLA-A allele frequencies
#' @param hlaB A data frame with HLA-B allele frequencies
#' @param hlaDR A data frame with HLA-DR allele frequencies
#' @param abo_freq A data frame with ABO blood group frequencies
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @param check.validity Logical to decide whether to validate input.
#' @return An ordered data frame with columns cPRA, HI, pointsET, SP, AM
#' and 'mmHLA'.
#' @examples
#' et(iso = TRUE, dABO = "A",
#' dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
#' donor.age = 65,
#' data = candidates, month = 2.78,
#' mm0 = 400, mm1 = 333.33, mm2 = 266.67, mm3 = 200,
#' mm4 = 133.33, mm5 = 66.67, mm6 = 0,
#' df.abs = cabs,
#' hlaA = hlaApt, hlaB = hlaBpt, hlaDR = hlaDRpt,
#' abo_freq = ABOpt,
#' n = 2,
#' check.validity = TRUE)
#' @export
et <- function(iso = TRUE
             , dABO = "A"
             , dA = c("1","2")
             , dB = c("15","44")
             , dDR = c("1","4")
             , donor.age = 65
             , data = candidates
             , month = 2.78
             , mm0 = 400
             , mm1 = 333.33
             , mm2 = 266.67
             , mm3 = 200
             , mm4 = 133.33
             , mm5 = 66.67
             , mm6 = 0
             , df.abs = cabs
             , hlaA = hlaApt
             , hlaB = hlaBpt
             , hlaDR = hlaDRpt
             , abo_freq = ABOpt
             , n = 2
             , check.validity = TRUE){

  if(check.validity){
    candidate_dataframe_check(data)
  }

  blood_group_checker(dABO) 
  age_checker(donor.age) 
  dialysis_checker(donor.age) 

  n <- max(1, n)

  xm <- xmatch(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs)
  data.table::setDT(xm, key = 'ID')

  data <- et_mmp(data = data, # Isto pode ser feito antes do for loop de candidato vs dador
                 hlaA = hlaA, hlaB = hlaB, hlaDR = hlaDR,
                 abo_freq = abo_freq)

  data <- data[, .(ID, bg, A1, A2, B1, B2, DR1, DR2,
                  age, dialysis, cPRA, urgent, MMP)]

  data[, ID := as.character(ID)] # ensure ID as a character
  xm[, ID := as.character(ID)] # ensure ID as a character

  data <- merge(data, xm, by = 'ID', all.x = TRUE)

  data[, `:=`(
    donor_age = donor.age,
    SP = ifelse(sp(donor.age = donor.age, candidate.age = age) == 1, 1, 0)
    ),
    by = 'ID'][, row_n := 1:nrow(data)]

  data[, `:=`(
    AM = ifelse(SP == 0 & cPRA >= 85, 1, 0)
    ),
    by = 'ID']

  data[, `:=`(
    compBlood = ifelse(AM == 1,
                       abo(iso = FALSE, dABO = dABO, cABO = bg),
                       abo(iso = iso, dABO = dABO, cABO = bg)
                       ),

    pointsDial = et_dialysis(month = month, dialysis = dialysis) # Isto pode ser feito antes do for loop de candidato vs dador
    ),
    by = 'ID']


  l <- list()

  for (i in 1:nrow(data)){
    res <- et_mmHLA(dA = dA,
                    dB = dB,
                    dDR = dDR,
                    cA = c(data$A1[i], data$A2[i]),
                    cB = c(data$B1[i], data$B2[i]),
                    cDR = c(data$DR1[i], data$DR2[i]),
                    mm0 = mm0,
                    mm1 = mm1,
                    mm2 = mm2,
                    mm3 = mm3,
                    mm4 = mm4,
                    mm5 = mm5,
                    mm6 = mm6
                    )

    l = append(l, res)
  }

  data[, `:=`(
    mmA = unlist(l[(1 + (row_n - 1) * 5)]),
    mmB = unlist(l[2 + (row_n - 1) * 5]),
    mmDR = unlist(l[3 + (row_n - 1) * 5]),
    mmHLA = unlist(l[4 + (row_n - 1) * 5]),
    pointsHLA = unlist(l[5 + (row_n - 1) * 5])
    )]

  data[, `:=`(
    mm000 = ifelse(mmA + mmB + mmDR == 0, 1, 0),
    pointsETx = round(pointsHLA + pointsDial + MMP)
    ),
    by = 'ID']

  data <- data[compBlood == TRUE & (xm == 'NEG' | is.na(xm)),][, `:=`(
    pointsET = ifelse(SP == 1, dialysis, pointsETx),
    HI = hiper(cPRA = cPRA, cutoff = 85)
    ),
    by = 'ID']

  return(
    data[order(-SP, -AM, -mm000, -pointsET)]
         [1:n]
         [!is.na(ID),]
         [, .(ID,
              bg,
              A1,
              A2,
              B1,
              B2,
              DR1,
              DR2,
              mmA,
              mmB,
              mmDR,
              mmHLA,
              age,
              donor_age,
              dialysis,
              cPRA,
              HI,
              pointsET,
              SP,
              AM)]
    )

}


