#' donor-recipient Risk Index Combination
#'
#' @description computes Risk Index Combination for each pair donor-recipient
#' @param DRI Donor RisK Index group (env$valid.dris)
#' @param data A data file with candidates information for UK transplant
#' @param D1R1 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D1R1
#' @param D1R2 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D1R2
#' @param D1R3 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D1R3
#' @param D1R4 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D1R4
#' @param D2R1 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D2R1
#' @param D2R2 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D2R2
#' @param D2R3 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D2R3
#' @param D2R4 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D2R4
#' @param D3R1 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D3R1
#' @param D3R2 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D3R2
#' @param D3R3 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D3R3
#' @param D3R4 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D3R4
#' @param D4R1 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D4R1
#' @param D4R2 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D4R2
#' @param D4R3 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D4R3
#' @param D4R4 A numeric value (`env$dirj.minimum` - `env$dirj.maximum`) for the combination of indexes D4R4
#' @param check.validity Logical to decide whether to validate input.
#' @return A tibble with a new column 'ric' that gives the  Risk Index Combination.
#' @examples
#' ric(DRI = 'D1', data = candidates.uk,
#' D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
#' D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
#' D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
#' D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000, check.validity = TRUE)
#' @export
ric <- function(DRI = 'D1',
              data = candidates.uk,
              D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
              D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
              D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
              D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000, check.validity = TRUE
) {
  if(check.validity){
    uk_candidate_dataframe_check(data)
  }

  if(!DRI %in% env$valid.dris){
      stop("Invalid DRI. Accepted values: ", env$valid.dris, ". \n")
    } else if(D1R1 < env$dirj.minimum | D1R1 > env$dirj.maximum){
      stop("D1R1 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D1R2 < env$dirj.minimum | D1R2 > env$dirj.maximum){
      stop("D1R2 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D1R3 < env$dirj.minimum | D1R3 > env$dirj.maximum){
      stop("D1R3 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D1R4 < env$dirj.minimum | D1R4 > env$dirj.maximum){
      stop("D1R4 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D2R1 < env$dirj.minimum | D2R1 > env$dirj.maximum){
      stop("D2R1 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D2R2 < env$dirj.minimum | D2R2 > env$dirj.maximum){
      stop("D2R2 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D2R3 < env$dirj.minimum | D2R3 > env$dirj.maximum){
      stop("D2R3 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D2R4 < env$dirj.minimum | D2R4 > env$dirj.maximum){
      stop("D2R4 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D3R1 < env$dirj.minimum | D3R1 > env$dirj.maximum){
      stop("D3R1 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D3R2 < env$dirj.minimum | D3R2 > env$dirj.maximum){
      stop("D3R2 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D3R3 < env$dirj.minimum | D3R3 > env$dirj.maximum){
      stop("D3R3 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D3R4 < env$dirj.minimum | D3R4 > env$dirj.maximum){
      stop("D3R4 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D4R1 < env$dirj.minimum | D4R1 > env$dirj.maximum){
      stop("D4R1 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D4R2 < env$dirj.minimum | D4R2 > env$dirj.maximum){
      stop("D4R2 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D4R3 < env$dirj.minimum | D4R3 > env$dirj.maximum){
      stop("D4R3 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    } else if(D4R4 < env$dirj.minimum | D4R4 > env$dirj.maximum){
      stop("D4R4 is not between ", env$dirj.minimum, " and " , env$dirj.maximum, "!\n")
    }

  if(DRI == 'D1') {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D1R1,
                                                          RRI == 'R2' ~ D1R2,
                                                          RRI == 'R3' ~ D1R3,
                                                          RRI == 'R4' ~ D1R4))
  } else if(DRI == 'D2') {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D2R1,
                                                          RRI == 'R2' ~ D2R2,
                                                          RRI == 'R3' ~ D2R3,
                                                          RRI == 'R4' ~ D2R4))
  } else if(DRI == 'D3') {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D3R1,
                                                          RRI == 'R2' ~ D3R2,
                                                          RRI == 'R3' ~ D3R3,
                                                          RRI == 'R4' ~ D3R4))
  } else {
    data <- data %>% dplyr::mutate(ric = dplyr::case_when(RRI == 'R1' ~ D4R1,
                                                          RRI == 'R2' ~ D4R2,
                                                          RRI == 'R3' ~ D4R3,
                                                          RRI == 'R4' ~ D4R4))
  }
  return(data)
}

#' Donor recipient age difference
#'
#' @description computes punctuation according to donor-recipient age difference
#' @param donor.age A numeric value with donor's age.
#' @param candidate.age A numeric value with candidate's age.
#' @return A numeric value.
#' @examples
#' age_diff(donor.age = 60, candidate.age = 50)
#' @export
age_diff <- function(donor.age = 60,
                   candidate.age = 50){
  if(!is.numeric(donor.age) | donor.age < env$adulthood.age | donor.age > env$person.maximum.age) {
    stop("Donor's age is not valid!\n")
  }
  if(!is.numeric(candidate.age) | candidate.age < env$adulthood.age | candidate.age > env$person.maximum.age) {
    stop("Candidate's age is not valid!\n")
  }
  
  res <- (-1 / 2) * ((donor.age - candidate.age) ^ 2)

  return(res)
}

#' blood group B match points
#'
#' @description computes penalization when donor's group O and candidate's group B
#' @param cABO A character from (`env$valid.blood.groups`)
#' @param dABO A character from (`env$valid.blood.groups`)
#' @param tier A character value for UK transplant TIER classification (`env$valid.tiers`)
#' @param pts A negative value with penalization for B candidates
#' @return A numeric value.
#' @examples
#' b_blood_penalization(dABO = "B", cABO = "O", tier = "B", pts = -1000)
#' @export
b_blood_penalization <- function(dABO = "B",
                  cABO = "O",
                  tier = "B",
                  pts = -1000){
  blood_group_checker(cABO)
  blood_group_checker(dABO)
  tier_checker(tier)
  if(!is.numeric(pts) | pts >= 0){
    stop('pts must be a negative value!')
  }
  
  res <- ifelse(cABO == 'B' & dABO == 'O' & tier == 'B', pts, 0)

  return(res)
}


#' test for ABO compatibility on UK transplant
#'
#' @description ABO compatibility test between donor and candidate according to
#' TIER classification
#' @param cABO A character from (`env$valid.blood.groups`), for candidate ABO group
#' @param dABO A character from (`env$valid.blood.groups`), for donor ABO group
#' @param tier A character value for UK transplant candidate's TIER classification (`env$valid.tiers`)
#' (options A and B)
#' @return A logical value T/F
#' @examples
#' abo_uk(dABO = "A", cABO = "A", tier = "B")
#' @export
abo_uk <- function(dABO = "A", 
                   cABO = "A", 
                   tier = "B"){
  blood_group_checker(dABO)
  blood_group_checker(cABO)
  tier_checker(tier)

  if(tier == 'B'){
    res <- ifelse(dABO == "O" & (cABO == "O" | cABO == "B"), TRUE,
              ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                ifelse(dABO == "B" & cABO == "B", TRUE,
                  ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                )
              )
            )
  } else {
    res <- ifelse(dABO == "O", TRUE,
              ifelse(dABO == "A" & (cABO == "A" | cABO == "AB"), TRUE,
                ifelse(dABO == "B" & cABO == "B", TRUE,
                  ifelse(dABO == "AB" & cABO == "AB", TRUE, FALSE)
                )
              )
            )
  }

  return(res)
}

#' resume function for UK algorithm punctuation
#'
#' @description Ordering of waitlisted candidates for a given donor and according
#' to UK transplant algorithm.
#' @param DRI Donor RisK Index group (env$valid.dris)
#' @param dABO A character value with ABO blood group (`env$valid.blood.groups`).
#' @param dA donor's HLA-A typing.
#' @param dB donor's HLA-B typing.
#' @param dDR donor's HLA-DR typing.
#' @param donor.age A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information for
#' a group of waitlisted transplant for UK transplant.
#' @param D1R1 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D1R2 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D1R3 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D1R4 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D2R1 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D2R2 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D2R3 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D2R4 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D3R1 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D3R2 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D3R3 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D3R4 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D4R1 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D4R2 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D4R3 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param D4R4 A numeric value (env$dirj.minimum - env$dirj.maximum) for the combination of indexes DiRj
#' @param ptsDial A numeric value for the points corresponding to each month
#' on dialysis
#' @param a1 A numeric value for HLA match and age combined formula:
#' b1*cos(age / env$adulthood.age)+a1
#' @param a2 A numeric value for HLA match and age combined formula:
#' b2*cos(age / env$adulthood.age)+a2
#' @param b1 A numeric value for HLA match and age combined formula:
#' b1*cos(age / env$adulthood.age)+a1
#' @param b2 A numeric value for HLA match and age combined formula:
#' b2*cos(age / env$adulthood.age)+a2
#' @param b3 A numeric value for HLA match and age combined formula:
#' b3*sin(age / 50)
#' @param m A numeric value for matchability formula: m * (1 + (MS / nn) ^ o)
#' @param nn A numeric value for matchability formula: m * (1 + (MS / nn) ^ o)
#' @param o A numeric value for matchability formula: m * (1 + (MS / nn) ^ o)
#' @param mm1 A numeric value to penalize 1 mm
#' @param mm23 A numeric value to penalize 2-3 mm
#' @param mm46 A numeric value to penalize 4-6 mm
#' @param pts A negative value with penalization for B candidates
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @param check.validity Logical to decide whether to validate input.
#' @examples
#' uk(DRI = 'D1', dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
#' dABO = "O", donor.age = 65, data = candidates.uk,
#' D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
#' D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
#' D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
#' D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000,
#' ptsDial = 1,
#' a1 = 2300,  a2 = 1500, b1 = 1200, b2 = 750, b3 = 400,
#' m = 40, nn = 4.5, o = 4.7,
#' mm1 = -100, mm23 = -150, mm46 = -250,
#' pts = -1000,
#' df.abs = cabs, n = 2, check.validity = TRUE)
#' @export
uk <- function(DRI = 'D1',
              dA = c("1","2"),
              dB = c("15","44"),
              dDR = c("1","4"),
              dABO = "O",
              donor.age = 65,
              data = candidates.uk,
              D1R1 = 1000,
              D1R2 = 700,
              D1R3 = 350,
              D1R4 = 0,
              D2R1 = 700,
              D2R2 = 1000,
              D2R3 = 500,
              D2R4 = 350,
              D3R1 = 350,
              D3R2 = 500,
              D3R3 = 1000,
              D3R4 = 700,
              D4R1 = 0,
              D4R2 = 350,
              D4R3 = 700,
              D4R4 = 1000,
              ptsDial = 1,
              a1 = 2300,
              a2 = 1500,
              b1 = 1200,
              b2 = 750,
              b3 = 400,
              m = 40,
              nn = 4.5,
              o = 4.7,
              mm1 = -100,
              mm23 = -150,
              mm46 = -250,
              pts = -1000,
              df.abs = cabs,
              n = 2,
              check.validity = TRUE){
  
  if(check.validity){
    uk_candidate_dataframe_check(data)
  }

  blood_group_checker(dABO)
  age_checker(donor.age)

  n <- max(1, n)

  xm <- xmatch(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs)

  data.table::setDT(data, key = 'ID')
  data.table::setDT(xm, key = 'ID')

   data <- ric(
    DRI = DRI, D1R1 = D1R1, D1R2 = D1R2, D1R3 = D1R3, D1R4 = D1R4,
    D2R1 = D2R1, D2R2 = D2R2, D2R3 = D2R3, D2R4 = D2R4,
    D3R1 = D3R1, D3R2 = D3R2, D3R3 = D3R3, D3R4 = D3R4,
    D4R1 = D4R1, D4R2 = D4R2, D4R3 = D4R3, D4R4 = D4R4,
    data = data
  )

   data[, ID := as.character(ID)] # ensure ID as a character
   xm[, ID := as.character(ID)] # ensure ID as a character

   data <- merge(data, xm,
                 by = 'ID',
                 all.x = TRUE)

   data[, `:=`(
      donor_age = donor.age,
      compBlood = abo_uk(dABO = dABO, cABO = bg, tier = Tier)
      ),
    by = 'ID'][, row_n := 1:nrow(data)]

  l <- list()

  for (i in 1:nrow(data)){
    res <- mmHLA(dA = dA,
                 dB = dB,
                 dDR = dDR,
                 cA = c(data$A1[i], data$A2[i]),
                 cB = c(data$B1[i], data$B2[i]),
                 cDR = c(data$DR1[i], data$DR2[i])
    )

    l <- append(l, res)
  }

  data[, `:=`(
    mmA = unlist(l[(1 + (row_n - 1) * 4)]),
    mmB = unlist(l[2 + (row_n - 1) * 4]),
    mmDR = unlist(l[3 + (row_n - 1) * 4]),
    mmHLA = unlist(l[4 + (row_n - 1) * 4])
  )]

  data[, `:=`(
    level = ifelse(mmA + mmB + mmDR == 0, 1,
              ifelse((mmDR == 0 & mmB <= 1) | (mmDR == 1 & mmB == 0), 2,
                ifelse((mmDR == 0 & mmB == 2) |(mmDR == 1 & mmB == 1), 3, 4)))
    ),
    by = 'ID']

  data[, `:=`(
    points.hla.age = ifelse(level == 1, b1 * cos(age / env$adulthood.age) + a1,
                    ifelse(level == 2, b2 * cos(age / env$adulthood.age) + a2,  b3 * sin(age / 50))),
    total.HLA = ifelse(mmA + mmB + mmDR == 0, 0,
                  ifelse(mmA + mmB + mmDR == 1, mm1,
                    ifelse(mmA + mmB + mmDR < 4, mm23, mm46))),
    # compute matchability points from Match Score
    matchability = round(m * (1 + (MS / nn) ^ o), 1), # Isto pode ser feito antes do for loop de candidato vs dador
    points.age = age_diff(donor.age = donor.age, candidate.age = age),
    points.abo = b_blood_penalization(dABO = dABO, cABO = bg, tier = Tier, pts = pts)
    ),
    by = 'ID']

  data[, `:=`(
    pointsUK = round(
        ifelse(Tier == "A",
               9999,
               ric + points.hla.age + matchability + points.age + total.HLA + points.abo)
        , 1)
    ),
    by = 'ID']

  return(
    data[compBlood == TRUE & (xm == 'NEG' | is.na(xm)),]
    [order(Tier, -pointsUK, -matchability, -dialysis)]
    [1:n]
    [!is.na(ID), .(ID,
                   bg,
                   A1,
                   A2,
                   B1,
                   B2,
                   DR1,
                   DR2,
                   matchability,
                   mmA,
                   mmB,
                   mmDR,
                   mmHLA,
                   age,
                   donor_age,
                   dialysis,
                   cPRA,
                   Tier,
                   pointsUK)]
    )
}
