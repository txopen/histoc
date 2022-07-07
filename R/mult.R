#' Candidates' selection according to any algorithm for multiple donors
#'
#' @description Ordering of waitlisted candidates for a given donor and
#' according to any algorithm.
#' @param df.donors A data frame containing demographics and medical information
#' for a poll of donors. For uk algorithm must have respective columns.
#' @param df.candidates A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates. For uk algorithm must have respective columns.
#' @param df.abs A data frame with candidates' antibodies.
#' @param algorithm The name of the function to use. Valid options are: lima, et, pt, uk (without quotation)
#' @param n A positive integer to slice the first candidates.
#' @param check.validity Logical to decide whether to validate input.
#' @param ... all the parameters used on the function algorithm
#' @return A list with the number of elements equal to the number of rows on donors data frame.
#' Each element have a data frame with select candidates by donor.
#' @examples
#' donor_recipient_pairs(df.donors = donors,
#' df.candidates = candidates,
#' df.abs = cabs,
#' algorithm = lima,
#' n = 2,
#' check.validity = TRUE)
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

  if(!identical(algorithm, uk) && !identical(algorithm, lima) && !identical(algorithm, pt) && !identical(algorithm, et)){
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

  df.donors <- df.donors %>%
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
    ) %>%
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

#' Runs several time the function donor_recipient_pairs() for as bootstrap.
#'
#' @description Generic function that runs the matchability between all combinations of donors and candidates.
#' Runs an arbitrary number of times ('iteration.number') to provide statistics.
#' @param iteration.number Number of times the matchability runs.
#' @param df.donors A data frame containing demographics and medical information
#' for a poll of donors. For uk algorithm must have respective columns.
#' @param df.candidates A data frame containing demographics and medical information
#' for a group of waitlisted transplant candidates. For uk algorithm must have respective columns.
#' @param df.abs A data frame with candidates' antibodies.
#' @param algorithm The name of the function to use. Valid options are: lima, et, pt, uk (without quotation)
#' @param n A positive integer to slice the first candidates.
#' @param seed.number Seed for new random number. seed.number can be NA in which case no seed is applied.
#' @param check.validity Logical to decide whether to validate input.
#' @param ... all the parameters used on the function algorithm
#' @return Statistics related to all the times the function ran.
#' @examples
#' several(iteration.number = 10,
#' df.donors = donors,
#' df.candidates = candidates,
#' df.abs = cabs,
#' algorithm = lima,
#' n = 0,
#' seed.number = 123,
#' check.validity = TRUE)
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

  mean_age <- all.statistics %>% purrr::map(., ~mean(.x$age)) %>% unlist()
  mean_dialysis <- all.statistics %>% purrr::map(., ~mean(.x$dialysis)) %>% unlist()
  mean_cPRA <- all.statistics %>% purrr::map(., ~mean(.x$cPRA)) %>% unlist()
  freq_mmHLA <- all.statistics %>% purrr::map(., ~table(.x$mmHLA)) #%>% unlist()
  freq_mmA <- all.statistics %>% purrr::map(., ~table(.x$mmA)) #%>% unlist()
  freq_mmB <- all.statistics %>% purrr::map(., ~table(.x$mmB)) #%>% unlist()
  freq_mmDR <- all.statistics %>% purrr::map(., ~table(.x$mmDR)) #%>% unlist()
  freq_ABO <- all.statistics %>% purrr::map(., ~table(.x$bg))
  freq_HI <- all.statistics %>% purrr::map(., ~table(.x$HI))
  freq_color <- all.statistics %>% purrr::map(., ~table(.x$cp))
  freq_SP <- all.statistics %>% purrr::map(., ~table(.x$SP))

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

uk_several <- function(){
  return(
    several(iteration.number = 10,
            df.donors = donors.uk,
            df.candidates = candidates.uk,
            df.abs = cabs,
            algorithm = uk,
            n = 0,
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
            pts = -1000
          )
    )
}

# No export, testing purposes
lima_several <- function(){
  return(
    several(
      iteration.number = 10,
      df.donors = donors,
      df.candidates = candidates,
      df.abs = cabs,
      algorithm = lima,
      n = 0, 
      function_name = "lima", 
      q2 = 60, 
      q3 = 100, 
      cPRA1 = 50, 
      cPRA2 = 85)
  )
}

# No export, testing purposes
et_several <- function(){
  return(
    several(
      iteration.number = 10,
      df.donors = donors,
      df.candidates = candidates,
      df.abs = cabs,
      algorithm = et,
      n = 0,
      iso = TRUE, 
      month = 2, 
      mm0 = 400, 
      mm1 = 333.33, 
      mm2 = 266.67, 
      mm3 = 200, 
      mm4 = 133.33, 
      mm5 = 66.67, 
      mm6 = 0, 
      hlaA = hlaApt, 
      hlaB = hlaBpt, 
      hlaDR = hlaDRpt, 
      abo_freq = ABOpt
    )
  )
}

# No export, testing purposes
pt_several <- function(){
  return(
    several(
      iteration.number = 10,
      df.donors = donors,
      df.candidates = candidates,
      df.abs = cabs,
      algorithm = pt,
      n = 0,
      iso = TRUE, 
      points.80 = 8, 
      points.50 = 4, 
      points.dialysis = 0.1, 
      points.age = 4)
  )
}

