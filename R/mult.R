#' Candidates' selection according to any algorithm for multiple donors
#'
#' @description Ordering of waitlisted candidates for a each donor in a pool of donors and
#' according to any algorithm.
#' @param df.donors A data frame containing demographics and medical information
#' for a pool of donors. For `uk` algorithm must have their respective columns.
#' @param df.candidates A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates. For `uk` algorithm must have respective columns.
#' @param df.abs A data frame with candidates' antibodies.
#' @param algorithm The name of the function to use. Valid options are:
#' \code{lima}, \code{et}, \code{pts}, \code{uk} (without quotation)
#' @param n A positive integer to slice the first candidates.
#' @param check.validity Logical to decide whether to validate input arguments.
#' @param ... all the parameters used on the algorithm function.
#' @return A list with the number of elements equal to the number of rows on donors' data frame.
#' Each element have a data frame with selected candidates by donor.
#' @examples
#' \donttest{
#' donor_recipient_pairs(df.donors = donors,
#' df.candidates = candidates,
#' df.abs = cabs,
#' algorithm = lima,
#' n = 2,
#' check.validity = TRUE)
#' }
#' @export
donor_recipient_pairs <- function(df.donors = donors,
                            df.candidates = candidates,
                            df.abs = cabs,
                            algorithm = lima,
                            n = 2,
                            check.validity = TRUE, ...){

  if(!is.numeric(n)){
    stop("'n' is not a valid numeric value!")
  }

  if(!identical(algorithm, uk) && !identical(algorithm, lima) && !identical(algorithm, pts) && !identical(algorithm, et)){
    stop("The algorithm doesn't exist.")
  }

  if(check.validity){
    if(identical(algorithm, uk)){
      uk_candidate_dataframe_check(df.candidates)
    }
    else{
      candidate_dataframe_check(df.candidates)
    }
  }

  df.donors <- df.donors |>
    dplyr::mutate(dABO = bg,
                  dA = purrr::map2(.x = A1,
                                   .y = A2,
                                   ~c(.x,.y)),
                  dB = purrr::map2(.x = B1,
                                   .y = B2,
                                   ~c(.x,.y)),
                  dDR = purrr::map2(.x = DR1,
                                    .y = DR2,
                                    ~c(.x,.y)),
                  donor.age = age
    ) |>
    dplyr::select(dABO, dA, dB, dDR, donor.age)

  if(n == 0) n <- nrow(df.candidates)

  lst <- purrr::pmap(df.donors,
                     data = df.candidates,
                     df.abs = df.abs,
                     algorithm,
                     n = n,
                     ...)

  names(lst) <- df.donors$ID

  return(lst)
}

#' Runs several time the function donor_recipient_pairs() as bootstrap.
#'
#' @description Generic function that runs the matchability between all combinations of donors and candidates.
#' Runs an arbitrary number of times (\code{iteration.number}) to provide statistics.
#' @param iteration.number Number of times the matchability runs.
#' @param df.donors A data frame containing demographics and medical information
#' for a pool of donors. For `uk` algorithm must have their respective columns.
#' @param df.candidates A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates. For `uk` algorithm must have respective columns.
#' @param df.abs A data frame with candidates' antibodies.
#' @param algorithm The name of the function to use. Valid options are:
#' \code{lima}, \code{et}, \code{pts}, \code{uk} (without quotation)
#' @param n A positive integer to slice the first candidates.
#' @param seed.number Seed for new random number.
#' \code{seed.number} can be \code{NA} in which case no seed is applied.
#' @param check.validity Logical to decide whether to validate input.
#' @param ... all the parameters used on the function algorithm
#' @return Overall statistics obtained from all runs.
#' @examples
#' \donttest{
#' several(iteration.number = 10,
#' df.donors = donors,
#' df.candidates = candidates,
#' df.abs = cabs,
#' algorithm = lima,
#' n = 0,
#' seed.number = 123,
#' check.validity = TRUE)
#' }
#' @export
several <- function(iteration.number = 10,
                       df.donors = donors,
                       df.candidates = candidates,
                       df.abs = cabs,
                       algorithm = lima,
                       n = 0,
                       seed.number = 123,
                       check.validity = TRUE, ...){

  if(check.validity){
    if(identical(algorithm, uk)){
      uk_candidate_dataframe_check(df.candidates)
    }
    else{
      candidate_dataframe_check(df.candidates)
    }

    if(!is.na(seed.number) && !is.numeric(seed.number)){
      stop("seed.number must either be NA or an integer.")
    }
  }

  if(!is.na(seed.number)){
    set.seed(seed.number)
  }

  df.donors$nrow <- 1:nrow(df.donors)

  pre_calculated_mappings <- donor_recipient_pairs(df.donors = df.donors,
                                                   df.candidates = df.candidates,
                                                   df.abs = df.abs,
                                                   algorithm = algorithm,
                                                   n = n, ...)

  all.statistics <- list()

  for (. in 1:iteration.number){
    used.candidates <- NULL
    current.iteration.statistics <- NULL
    shuffled_donors <- sample(df.donors$nrow)

    for (j in 1:length(shuffled_donors)){
      tmp <- pre_calculated_mappings[[shuffled_donors[j]]][
        !ID %in% used.candidates,][
          1:2,]

      current.iteration.statistics <- data.table::rbindlist(list(current.iteration.statistics,
                                                                 tmp))

      used.candidates <- c(used.candidates, tmp$ID)
    }

    all.statistics <- append(all.statistics, list(current.iteration.statistics))
  }

  mean_age <- all.statistics |> purrr::map(., ~mean(.x$age)) |> unlist()
  mean_dialysis <- all.statistics |> purrr::map(., ~mean(.x$dialysis)) |> unlist()
  mean_cPRA <- all.statistics |> purrr::map(., ~mean(.x$cPRA)) |> unlist()
  freq_mmHLA <- all.statistics |> purrr::map(., ~table(.x$mmHLA))
  freq_mmA <- all.statistics |> purrr::map(., ~table(.x$mmA))
  freq_mmB <- all.statistics |> purrr::map(., ~table(.x$mmB))
  freq_mmDR <- all.statistics |> purrr::map(., ~table(.x$mmDR))
  freq_ABO <- all.statistics |> purrr::map(., ~table(.x$bg))
  freq_HI <- all.statistics |> purrr::map(., ~table(.x$HI))
  freq_color <- all.statistics |> purrr::map(., ~table(.x$cp))
  freq_SP <- all.statistics |> purrr::map(., ~table(.x$SP))

  return(list(age = mean_age,
              dialysis = mean_dialysis,
              cPRA = mean_cPRA,
              mmHLA = freq_mmHLA,
              mmA = freq_mmA,
              mmB = freq_mmB,
              mmDR = freq_mmDR,
              ABO = freq_ABO,
              HI = freq_HI,
              color = freq_color,
              SP = freq_SP)
        )
}
