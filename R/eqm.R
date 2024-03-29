#' Utility - Justice matrix
#'
#' @description Creates a matrix with scores for utility and justice criteria on organ transplantation
#' @param max.val A numerical value corresponding o the matrix maximum value.
#' @param ratio.util A numerical value between 0 and 0.5 corresponding to the decreasing rate for the utility criterion (by row)
#' @param ratio.just A numerical value between 0 and 0.5 corresponding to the decreasing rate for the justice criterion (by column)
#' @return A 6 by 6 matrix with at line 1 row 1 has the maximum value and at line 6, row 6 has it's minimum.
#' @export
#' @examples
#' uj_matx(max.val = 100, ratio.util = 0.1, ratio.just = 0.1)
uj_matx <- function(max.val = 100,
                    ratio.util = 0.1,
                    ratio.just = 0.1){

  if(!(ratio.just > 0 & ratio.just <= 0.5 &
       ratio.util > 0 & ratio.util <= 0.5)
  ){stop('ratio values are not between 0 and 0.5!')}

  if(!(max.val > 0)){stop('max.val is not greater than 0!')}

  matx <- matrix(nrow = 6,
                 ncol = 6)

  util.val <- max.val * ratio.util
  just.val <- max.val * ratio.just

  l1 <- max.val
  for(i in 1:5){l1 <- c(l1,max.val - util.val*i)}
  c1 <- max.val
  for(i in 1:5){c1 <- c(c1,max.val - just.val*i)}

  matx[1,] <- l1
  matx[,1] <- c1

  for(i in 2:6){
    for(j in 2:6){
      matx[i,j] <- matx[i, j-1] - util.val
    }
  }

  if( is.na(sum(matx)) | is.nan(sum(matx)) ){stop("The matrix have non valid numbers!")}

  matx0 <- matrix(0, ncol = 6, nrow = 6)
  if(identical(matx,matx0) ){stop("All matrix values are zeros!")}

  return(matx)
}

#' Equity matrix algorithm
#'
#' @description An algorithm to apply on deceased donor's Kidney allocation for transplantation.
#' Ordering of wait listed candidates for a given donor and according to an utility-justice matrix.
#' @param iso A logical value for ABO isogroupal compatibility.
#' @param dABO A character value with donor's ABO blood group (`r env$valid.blood.groups`).
#' @param dA A two elements character vector donor's HLA-A typing.
#' @param dB A two elements character vector donor's HLA-B typing.
#' @param dDR A two elements character vector donor's HLA-DR typing.
#' @param donor.age A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates.
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @param q2 A numerical value for the median of candidates' waiting list (`r env$q.minimum` - `r env$q.maximum`).
#' @param q3 A numerical value for the third quartile of candidates' waiting list (`r env$q.minimum` - `r env$q.maximum`).
#' @param uj.matx A 6 by 6 matrix obtained from \code{uj_matx()}.
#' @param SP A logical value, when `TRUE` donors with 65+ years old are allocated prioritize to 65+ candidates,similarly do the Senior Program.
#' @param AM A logical value, when `TRUE` hipersensitized candidates are prioritize, similarly to the Acceptable mismatch program.
#' @param mm000 A logical value, when `TRUE` candidates with 0 HLA mismatchs with the donor are prioritize.
#' @param check.validity Logical to decide whether to validate input arguments.
#' @return An ordered data frame with a column \code{ptsEQM} (points from utility-justice matrix), \code{SP}, \code{HI} and \code{mmHLA}.
#' @export
#' @examples
#' \dontrun{
#' eqm(iso = TRUE, dABO = "O" ,
#' dA = c("1","2"), dB = c("15","44"), dDR = c("1","4") ,
#' donor.age = 60 , df.abs = cabs , data = candidates ,
#' n = 2 , q2 = 60 , q3 = 80 , uj.matx = uj_matx(),
#' SP = FALSE, AM = FALSE, mm000 = FALSE,
#' check.validity = FALSE)
#' }
eqm <- function(iso = TRUE
                , dABO = "O"
                , dA = c("1","2"), dB = c("15","44"), dDR = c("1","4")
                , donor.age = 60
                , df.abs = cabs
                , data = candidates
                , n = 2
                , q2 = 60
                , q3 = 80
                , uj.matx = uj_matx()
                , SP = FALSE
                , AM = FALSE
                , mm000 = FALSE
                , check.validity = FALSE){

  if(check.validity){
    candidate_dataframe_check(data)
  }
  age_checker(donor.age)
  blood_group_checker(dABO)

  if(q2 >= q3){
    stop("q2 should be smaller than q3. q2 was ", q2, " and q3 was ", q3)
  }
  if(#q2 < 1 || q3 < 1 || q2 > 120 || q3 > 120){
    q2 < env$q.minimum || q3 < env$q.minimum || q2 > env$q.maximum || q3 > env$q.maximum){
    stop("q2 and q3 should be bigger or equal to ", env$q.minimum, " an smaller or equal to ", env$q.maximum)
    #stop("check q2 and q3 values!")
  }

  if(!is.numeric(n) | n < 0){stop('n must be an positive number!')}
  n <- floor(n)
  if(n == 0) n <- nrow(data)
  #n <- max(1, n)

  xm <- xmatch(dA = dA, dB = dB, dDR = dDR, df.abs = df.abs)

  data.table::setDT(data, key = 'ID')
  data.table::setDT(xm, key = 'ID')

  data[, ID := as.character(ID)]
  xm[, ID := as.character(ID)]

  data <- merge(data, xm,
                by = 'ID',
                all.x = TRUE)

  data[, `:=`(
    donor_age = donor.age,
    SP = ifelse(sp(donor.age = donor.age, candidate.age = age) == 1, 1, 0),
    HI = hiper(cPRA = cPRA, 85),
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
    l <- append(l, res)
  }

  data[, `:=`(
    mmA = unlist(l[(1 + (row_n - 1) * 4)]),
    mmB = unlist(l[2 + (row_n - 1) * 4]),
    mmDR = unlist(l[3 + (row_n - 1) * 4]),
    mmHLA = unlist(l[4 + (row_n - 1) * 4])
  )]


  data[, `:=` (co = data.table::fcase(abs(donor_age - age) < 9 &  mmHLA <3 , 1,
                                      abs(donor_age - age) < 9 &  mmHLA <5 , 2,
                                      abs(donor_age - age) >= 9 &  mmHLA <3 , 3,
                                      abs(donor_age - age) < 9 &  mmHLA >4 , 4,
                                      abs(donor_age - age) >= 9 &  mmHLA <5 , 5,
                                      abs(donor_age - age) >= 9 &  mmHLA >4 , 6),
               ro = data.table::fcase(dialysis > q3 & cPRA > 50 , 1,
                                      dialysis > q3 & cPRA <= 50 , 2,
                                      dialysis > q2 & cPRA > 50 , 3,
                                      dialysis > q2 & cPRA <= 50 , 4,
                                      dialysis <= q2 & cPRA > 50 , 5,
                                      dialysis <= q2 & cPRA <= 50 , 6)
  ),
  by = 'ID'][, ptsEQM := uj.matx[ro,co],
             by = 'ID'][, `:=` (AM = ifelse(SP == 0 & HI, 1, 0),
                                mm000 = ifelse(mmHLA == 0, 1, 0)),
                        by = 'ID']

  data <- data[compBlood == TRUE & (xm == 'NEG' | is.na(xm)),][]

  if(!SP & !AM & !mm000){#
    data <- data[order(-urgent, -ptsEQM, mmHLA, -dialysis)][]
  } else if(SP & !AM & !mm000){#
    data <- data[order(-urgent, -SP, -ptsEQM, mmHLA, -dialysis)][]
  } else if(SP & AM & !mm000){#
    data <- data[order(-urgent, -SP, -AM, -ptsEQM, mmHLA, -dialysis)][]
  } else if(SP & AM & mm000){#
    data <- data[order(-urgent, -SP, -AM, -mm000, -ptsEQM, mmHLA, -dialysis)][]
  } else if(!SP & AM & !mm000){#
    data <- data[order(-urgent, -AM, -ptsEQM, mmHLA, -dialysis)][]
  } else if(!SP & AM & mm000){#
    data <- data[order(-urgent, -AM, -mm000, -ptsEQM, mmHLA, -dialysis)][]
  } else if(!SP & !AM & mm000){#
    data <- data[order(-urgent, -mm000, -ptsEQM, mmHLA, -dialysis)][]
  } else if(SP & !AM & mm000){#
    data <- data[order(-urgent, -SP, -mm000, -ptsEQM, mmHLA, -dialysis)][]
  }

  return(data[1:n,][
      !is.na(ID),][,
                   .(ID,
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
                     ptsEQM,
                     SP,
                     AM,
                     urgent)]
      )

}
