################# Field validation functions #################

#' Tests if element is a valid Blood Group character
#' @param input_string A character from `r env$valid.blood.groups`
#' @noRd
blood_group_checker <- function(input_string){
  if(!input_string %in% env$valid.blood.groups){
      stop("Invalid blood group. Accepted values:", env$valid.blood.groups)
  }
}

#' Validates that the age of a person is not negative.
#' @param input_number A numeric value between `r env$person.minimum.age` and `r env$person.maximum.age`
#' @noRd
age_checker <- function(input_number){
  if(!is.numeric(input_number)){
    stop("Invalid age: Age should be a number.")
  }

  if(input_number < env$person.minimum.age){
    stop("Invalid age: Age should be higher or equal to ", env$person.minimum.age, ".")
  }

  if(input_number > env$person.maximum.age){
    stop("Invalid age: Age should be lower or equal to ", env$person.maximum.age, ".")
  }
}

#' Validates the time someone spent on dialysis.
#' @param input_number A numeric value between `r env$dialysis.minimum` and `r env$dialysis.maximum`
#' @noRd
dialysis_checker <- function(input_number){
  if(!is.numeric(input_number)){
    stop("Invalid dialysis: Dialysis should be a number.")
  }

  if(input_number < env$dialysis.minimum){
    stop("Invalid dialysis: Dialysis should be higher or equal to ", env$dialysis.minimum, ".")
  }

  if(input_number > env$dialysis.maximum){
    stop("Invalid dialysis: Dialysis should be lower or equal to ", env$dialysis.maximum, ".")
  }
}

#' Tests if element is a valid Tier character
#' @param input_string A character from `r env$valid.tiers`
#' @noRd
tier_checker <- function(input_string){
  if(!input_string %in% env$valid.tiers){
    stop("Invalid tier. Accepted values: ", env$valid.tiers)
  }
}

#' Validates that the RRI is within the correct range of values
#' @param input_string A character from `r env$valid.rris`
#' @noRd
rri_checker <- function(input_string){
  if(!input_string %in% env$valid.rris){
    stop("Invalid rris. Accepted values: ", env$valid.rris)
  }
}

#' Validates that the urgent is within the correct range of values
#' @param input_number A character from `r env$valid.urgent`
#' @noRd
urgent_checker <- function(input_number){
  if(!is.numeric(input_number) || !input_number %in% env$valid.urgent){
    stop("Invalid urgent. Accepted values: ", env$valid.urgent)
  }
}

#' Validates that cPRA is within the correct range of values
#' @param input_number A numeric value between `r env$cPRA.minimum` and `r env$cPRA.maximum`
#' @noRd
cPRA_checker <- function(input_number){
  if(!is.numeric(input_number)){
    stop("Invalid cPRA: cPRA should be a number.")
  }

  if(input_number < env$cPRA.minimum){
    stop("Invalid cPRA: cPRA should be higher or equal to ", env$cPRA.minimum, ".")
  }

  if(input_number > env$cPRA.maximum){
    stop("Invalid age: cPRA should be lower or equal to ", env$cPRA.maximum, ".")
  }
}

################# Field validation functions #################

################# File / Dataframe validation functions #################

#' Validates the Candid file.
#' Makes sure the header matches the header that a candid file should have.
#' For each line, call blood group and age checks.
#' @param candidate.dataframe a dataframe
#' @return A logical value T/F
#' @noRd
candidate_dataframe_check <- function(candidate.dataframe){
  candidate.fields <- c(
    'ID',
    'bg',
    'A1',
    'A2',
    'B1',
    'B2',
    'DR1',
    'DR2',
    'age',
    'dialysis',
    'cPRA',
    'urgent')

  for (i in 1:length(candidate.fields)){
    if(!candidate.fields[i] %in% colnames(candidate.dataframe)){
      stop('Column ', candidate.fields[i], ' is not present in the file.')
    }
  }

  if(length(candidate.fields) != length(colnames(candidate.dataframe))){
    stop('There are unexpected columns in the file. Expected: ', candidate.fields, ' ', collapse = ", ")
  }

  candidate.datatable <- data.table::setDT(rlang::duplicate(candidate.dataframe), key = 'ID')
  duplication.location <- anyDuplicated(candidate.datatable)

  if(duplication.location != 0){
    stop(paste('Duplicated ID in line', duplication.location))
  }

  for (i in 1:nrow(candidate.dataframe)){
    blood_group_checker(candidate.dataframe$bg[i])
    age_checker(candidate.dataframe$age[i])
    cPRA_checker(candidate.dataframe$cPRA[i])
    dialysis_checker(candidate.dataframe$cPRA[i])
    urgent_checker(candidate.dataframe$urgent[i])
  }

  return(TRUE)
}

#' Validates the CandidUK file.
#' Makes sure the header matches the header that a candid file should have.
#' For each line, call blood group and age checks.
#' @param candidate.dataframe candidate's dataframe
#' @return A logical value T/F
#' @noRd
uk_candidate_dataframe_check <- function(candidate.dataframe){
  candid_uk_columns <- c(
    'ID',
    'bg',
    'A1',
    'A2',
    'B1',
    'B2',
    'DR1',
    'DR2',
    'age',
    'dialysis',
    'cPRA',
    'Tier',
    'MS',
    'RRI',
    'urgent')

  for (i in 1:length(candid_uk_columns)){
    if(!candid_uk_columns[i] %in% colnames(candidate.dataframe)){
      stop(paste('Column', candid_uk_columns[i], 'is not present in the file.'))
    }
  }

  if(length(candid_uk_columns) != length(colnames(candidate.dataframe))){
    stop('There are unexpected columns in the file. Expected:\n', paste(candid_uk_columns, collapse = ", "))
  }

  candidate.datatable <- data.table::setDT(rlang::duplicate(candidate.dataframe), key = 'ID')
  duplication.location <- anyDuplicated(candidate.datatable)

  if(duplication.location != 0){
    stop(paste('Duplicated ID in line', duplication.location))
  }

  for (i in 1:nrow(candidate.dataframe)){
    blood_group_checker(candidate.dataframe$bg[i])
    tier_checker(candidate.dataframe$Tier[i])
    age_checker(candidate.dataframe$age[i])
    rri_checker(candidate.dataframe$RRI[i])
    cPRA_checker(candidate.dataframe$cPRA[i])
    dialysis_checker(candidate.dataframe$cPRA[i])
    urgent_checker(candidate.dataframe$urgent[i])
  }

  return(TRUE)
}
