#' Points for age differences
#'
#' @description Punctuation given for age difference between candidates and donors
#' @param donor.age A numeric value with donor's age.
#' @param candidate.age A numeric value with candidate's age.
#' @param age.difference.points A numerical value for the points to age difference
#' @return A numerical value for pre-defined points
#' @examples
#' pts_age(donor.age = 60, candidate.age = 40, age.difference.points = 4)
#' @export
pts_age <- function(donor.age = 60
                    , candidate.age = 40
                    , age.difference.points = 4){
  if(!is.numeric(donor.age) | donor.age < env$adulthood.age | donor.age > env$person.maximum.age){
    stop("Donor's age is not valid!\n")}
  if(!is.numeric(candidate.age) | candidate.age < env$adulthood.age | candidate.age > env$person.maximum.age){
    stop("Candidate's age is not valid!\n")}
  if(!is.numeric(age.difference.points) | age.difference.points < env$minimum.age.difference.points | age.difference.points > env$maximum.age.difference.points){
    stop("Age points are not valid!\n")}

  age.difference.points <- ifelse(
    (
      (donor.age > 60 & candidate.age < 55) | 
      (donor.age < 40 & candidate.age > 55)
    ),
    0,
    age.difference.points
  )

  return(age.difference.points)
}

#' Points for cPRA sensitization
#'
#' @description Punctuation given for sensitized patients according to cPRA value
#' @param cPRA Percentual value of cPRA (0 - 100)
#' @param points.80 A numerical value (`env$pt.points.minimum` - `env$pt.points.maximum`) for the points to a cPRA >= 80
#' @param points.50 A numerical value (`env$pt.points.minimum` - `env$pt.points.maximum`) for the points to a cPRA >= 50
#' @return A numerical value for pre-defined points
#' @examples
#' pts_PRA(cPRA = 0, points.80 = 8, points.50 = 4)
#' @export
pts_PRA <- function(cPRA = 0
                    , points.80 = 8
                    , points.50 = 4){
  if(!is.numeric(cPRA) | cPRA < 0 | cPRA > 100){
    stop("PRA value is not valid!\n")}
  if(!is.numeric(points.80) | points.80 < 0 | points.80 > 20){
    stop("attributed points for a PRA >= 80% is not valid!\n")}
  if(!is.numeric(points.50) | points.50 < 0 | points.50 > 20){
    stop("attributed points for a PRA >= 50% is not valid!\n")}

  pts <- dplyr::if_else(cPRA >= 80, points.80,
                      dplyr::if_else(cPRA >= 50, points.50, 0))

  return(pts)
}

#' Points for HLA mismatches
#'
#' @description Punctuation given according to HLA mismatchs (mm) for item A) to E) from PT's algorithm
#' @param itemA Points for HLA fullmatch (no mm for HLA-A, B and DR)
#' @param itemB Points without mm for HLA-B and DR
#' @param itemC Points with 1 mm for HLA-B and DR
#' @param itemD Points with 1 mm for HLA-B and 1 mm for DR
#' @param itemE Points for remaing possibilities
#' @param mm.A Number of HLA-A mismatchs(0 to 2)
#' @param mm.B Number of HLA-B mismatchs(0 to 2)
#' @param mm.DR Number of HLA-DR mismatchs(0 to 2)
#' @return A numerical value for pre-defined points
#' @examples
#' pts_HLA(itemA = 12, itemB = 8, itemC = 4, itemD = 2, itemE = 1
#' , mm.A = 0, mm.B = 0, mm.DR = 0)
#' @export
pts_HLA <- function(itemA = 12
                    , itemB = 8
                    , itemC = 4
                    , itemD = 2
                    , itemE = 1
                    , mm.A = 0
                    , mm.B = 0
                    , mm.DR = 0){
  if(!is.numeric(itemA) | itemA < 0 | itemA > 99){
    stop("points for 0 mmHLA (full match) is not valid!\n")}
  if(!is.numeric(itemB) | itemB < 0 | itemB > 99){
    stop("points for 0 mmB and mmDR is not valid!\n")}
  if(!is.numeric(itemC) | itemC < 0 | itemC > 99){
    stop("points for 1 mmB or mmDR is not valid!\n")}
  if(!is.numeric(itemD) | itemD < 0 | itemD > 99){
    stop("points for 1 mmB and 1 mmDR is not valid!\n")}
  if(!is.numeric(itemE) | itemE < 0 | itemE > 99){
    stop("points for more than 2 mmB and mmDR is not valid!\n")}


  mm <- list('mmA' = mm.A,
             'mmB' = mm.B,
             'mmDR' = mm.DR,
             'mmHLA' = mm.A + mm.B + mm.DR)

  pts <- dplyr::if_else(mm[["mmHLA"]] == 0, itemA,
                      dplyr::if_else(mm[["mmB"]]+mm[["mmDR"]] == 0, itemB,
                                     dplyr::if_else(mm[["mmB"]]+mm[["mmDR"]] == 1, itemC,
                                                    dplyr::if_else(mm[["mmB"]] == 1 & mm[["mmDR"]] == 1, itemD,
                                       itemE))))
  return(pts)
}

#' Matching punctuation' according to 2007 PT's algorithm
#'
#' @description Ordering of waitlisted candidates for a given donor and
#' according to PT's algorithm.
#' @param iso A logical value for isogroupal compatibility.
#' @param dABO A character value with ABO blood group (`env$valid.blood.groups`).
#' @param dA donor's HLA-A typing.
#' @param dB donor's HLA-B typing.
#' @param dDR donor's HLA-DR typing.
#' @param donor.age A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information for
#' a group of waitlisted transplant candidates with color priority classification.
#' @param df.abs A data frame with candidates' antibodies.
#' @param points.80 A numerical value (`env$pt.points.minimum` - `env$pt.points.maximum`) for the points to a cPRA >= 80
#' @param points.50 A numerical value (`env$pt.points.minimum` - `env$pt.points.maximum`) for the points to a cPRA >= 50
#' @param points.dialysis punctuaction for each month on dialysis
#' @param points.age A numerical value for the points to age difference
#' @param n A positive integer to slice the first candidates.
#' @param check.validity Logical to decide whether to validate input.
#' @return An ordered data frame with a column 'cp' (color priority),
#' 'sp', 'hi' and 'mmHLA'.
#' @examples
#' pt(iso = TRUE, dABO = "A",
#' dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
#' donor.age = 65,  data = candidates,
#' df.abs = cabs, n = 2)
#' @export
pt <- function(iso = TRUE
                , dABO = "O"
                , dA = c("1","2")
                , dB = c("15","44")
                , dDR = c("1","4")
                , donor.age = 65
                , df.abs = cabs
                , data = candidates
                , points.80 = 8
                , points.50 = 4
                , points.dialysis = 0.1
                , points.age = 4
                , n = 2
                , check.validity = TRUE){
  
  if(check.validity){
    candidate_dataframe_check(candidates)
  }

  if(points.80 < env$pt.points.minimum || points.80 > env$pt.points.maximum){
    stop("points.80 should be between ", env$pt.points.minimum, " and ", env$pt.points.maximum)
  }
  if(points.50 < env$pt.points.minimum || points.50 > env$pt.points.maximum){
    stop("points.50 should be between ", env$pt.points.minimum, " and ", env$pt.points.maximum)
  }
  if(points.dialysis < env$points.dialysis.minimum || points.dialysis > env$points.dialysis.maximum){
    stop("points.dialysis should be between ", env$points.dialysis.minimum, " and ", env$points.dialysis.maximum)
  }
  if(points.age < env$points.age.minimum || points.age > env$points.age.maximum){
    stop("points.age should be between ", env$points.age.minimum, " and ", env$points.age.maximum)
  }

  n <- max(1, n)

  data <- cp(data = data) %>% # Isto pode ser feito antes do for loop de candidato vs dador
    as.data.frame()

  xm <- xmatch(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs)

  data.table::setDT(data, key = 'ID')
  data.table::setDT(xm, key = 'ID')

  data <- merge(data, xm,
                by = 'ID',
                all.x = TRUE)

    data[, `:=`(
      donor_age = donor.age,
      SP = sp(candidate.age = age, donor.age = donor.age),
      HI = hiper(cPRA = cPRA),
      compBlood = abo(iso = iso, dABO = dABO, cABO = bg)
      ), by = 'ID'][, row_n := 1:nrow(data)]

  l <- list()

  for (i in 1:nrow(data)){
    res <- mmHLA(dA = dA,
                 dB = dB,
                 dDR = dDR,
                 cA = c(data$A1[i], data$A2[i]),
                 cB = c(data$B1[i], data$B2[i]),
                 cDR = c(data$DR1[i], data$DR2[i])
    )

    l = append(l, res)
  }

  data[, `:=`(
    mmA = unlist(l[(1 + (row_n - 1) * 4)]),
    mmB = unlist(l[2 + (row_n - 1) * 4]),
    mmDR = unlist(l[3 + (row_n - 1) * 4]),
    mmHLA = unlist(l[4 + (row_n - 1) * 4])
  )]

  data[, `:=`(
      ptsHLA = pts_HLA(mm.A = mmA, mm.B = mmB, mm.DR = mmDR),
      ptsPRA = pts_PRA(cPRA = cPRA, points.80 = points.80, points.50 = points.50), # Isto pode ser feito antes do for loop de candidato vs dador
      ptsage = pts_age(donor.age = donor.age, candidate.age = age, age.difference.points = points.age),
      ptsdial = points.dialysis * dialysis # Isto pode ser feito antes do for loop de candidato vs dador
      ), by = 'ID']

    data[, `:=`(
      ptsPT = ptsHLA + ptsPRA + ptsage + ptsdial
      ), by = 'ID']

  return(
    data[compBlood == TRUE & (xm == 'NEG' | is.na(xm)),]
    [order(HI, -ptsPT)]
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
         ptsPT, 
         SP,
         ptsHLA, 
         ptsPRA, 
         ptsage, 
         ptsdial)
    ]
  )
}
