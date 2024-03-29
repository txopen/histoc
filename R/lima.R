#' Lima's algorithm
#'
#' @description Applies Lima's algorithm on deceased donor's Kidney allocation for transplantation.
#' Ordering of waitlisted candidates for a given donor and
#' according to Lima's algorithm.
#' @param iso A logical value for isogroupal compatibility.
#' @param dABO A character value with donor's ABO blood group (`r env$valid.blood.groups`).
#' @param dA A two elements character vector donor's HLA-A typing.
#' @param dB A two elements character vector donor's HLA-B typing.
#' @param dDR A two elements character vector donor's HLA-DR typing.
#' @param donor.age A numeric value with donor's age.
#' @param data A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates with
#' color priority classification.
#' @param df.abs A data frame with candidates' antibodies.
#' @param n A positive integer to slice the first candidates.
#' @param q2 A numerical value for the median of candidates' waiting list (`r env$q.minimum` - `r env$q.maximum`).
#' @param q3 A numerical value for the third quartile of candidates' waiting list (`r env$q.minimum` - `r env$q.maximum`).
#' @param cPRA1 A numerical value (`r env$percentage.minimum` - `r env$percentage.maximum`) for the lower cPRA cutoff.
#' @param cPRA2 A numerical value (`r env$percentage.minimum` - `r env$percentage.maximum`) for the higher cPRA cutoff.
#' @param check.validity Logical to decide whether to validate input arguments.
#' @return An ordered data frame with a column \code{cp} (color priority),
#' \code{SP}, \code{HI} and \code{mmHLA}.
#' @examples
#' \dontrun{
#' lima(iso = TRUE, dABO = "O",
#' dA = c("1","2"), dB = c("15","44"), dDR = c("1","4"),
#' donor.age = 60, df.abs = cabs,
#' data = candidates, n = 2, check.validity = TRUE)
#' }
#' @export
lima <- function(iso = TRUE
                  , dABO = "O"
                  , dA = c("1","2"), dB = c("15","44"), dDR = c("1","4")
                  , donor.age = 60
                  , df.abs = cabs
                  , data = candidates
                  , n = 2
                  , q2 = 60
                  , q3 = 100
                  , cPRA1 = 50
                  , cPRA2 = 85
                  , check.validity = TRUE){

  if(check.validity){
    candidate_dataframe_check(data)
  }

  age_checker(donor.age)
  if(q2 >= q3){
    stop("q2 should be smaller than q3. q2 was ", q2, " and q3 was ", q3)
  }
  if(q2 < env$q.minimum || q3 < env$q.minimum || q2 > env$q.maximum || q3 > env$q.maximum){
    stop("q2 and q3 should be bigger or equal to ", env$q.minimum, " an smaller or equal to ", env$q.maximum)
  }

  if(!is.numeric(n) | n < 0){stop('n must be an positive number!')}
  n <- floor(n)
  if(n == 0) n <- nrow(data)

  data <- cp(data = data
            , q2 = q2
            , q3 = q3
            , cPRA1 = cPRA1
            , cPRA2 = cPRA2) |>
    as.data.frame()

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

  return(
    data[compBlood == TRUE & (xm == 'NEG' | is.na(xm)) & SP < 3,]
    [order(-SP, cp, mmHLA, -dialysis)]
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
         cp,
         SP,
         urgent)
    ]
  )
}

#' Candidates' Color Priority
#'
#' @description Classification of candidates according to waiting list
#' time on dialysis' quartiles and two cPRA cutoff values.
#' @param data A data frame with information for candidates' waiting list.
#' @param q2 A numerical value for the median of candidates' waiting list (`r env$q.minimum` - `r env$q.maximum`).
#' @param q3 A numerical value for the third quartile of candidates' waiting list (`r env$q.minimum` - `r env$q.maximum`).
#' @param cPRA1 A numerical value (`r env$percentage.minimum` - `r env$percentage.maximum`) for the lower cPRA cutoff.
#' @param cPRA2 A numerical value (`r env$percentage.minimum` - `r env$percentage.maximum`) for the higher cPRA cutoff. cPRA2
#' must be greater than cPRA1.
#' @return A data frame with a new column 'cp' (color priority)
#' @examples
#' cp(data = candidates, q2 = 60, q3 = 100, cPRA1 = 50, cPRA2 = 85)[1:5,]
#' @export
cp <- function(data = candidates,
               q2 = 60,
               q3 = 100,
               cPRA1 = 50,
               cPRA2 = 85){
  if(cPRA2 < cPRA1){
    stop("Higher cPRA cutoff value (cPRA2) must be greater than lower cPRA cutoff (cPRA1)!\n")
  }
  if(cPRA1 > env$percentage.maximum || cPRA1 < env$percentage.minimum){
    stop("cPRA1 corresponds to a percetage. Values should be between ",
         env$percentage.maximum, " and ", env$percentage.minimum, ".")
  }
  if(cPRA2 > env$percentage.maximum || cPRA2 < env$percentage.minimum){
    stop("cPRA2 corresponds to a percetage. Values should be between ",
         env$percentage.maximum, " and ", env$percentage.minimum, ".")
  }
  if(q2 >= q3){
    stop("Median time on dialysis quartiles must be lower than third quartile: q2 < q3!\n")
  }

  data <- data |>
    dplyr::mutate(cp = ifelse(urgent == 1, 1,
                              ifelse(cPRA >= cPRA2 | dialysis >= q3, 2,
                                     ifelse(cPRA >= cPRA1 | dialysis >= q2, 3, 4)
                              )
    ),
    cp = factor(cp, levels = 1:4,
                labels = env$color.priority.labels
    )
    )

  return(data)
}
