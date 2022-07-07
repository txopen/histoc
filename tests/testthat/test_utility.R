test_that('Test Blood Group Checker', {
  valid_input <- c('O', 'A', 'B', 'AB')
  invalid_input <- c('o', 'OA', 'Z', 1)
  
  for (valid in valid_input){
    blood_group_checker(valid)
  }
  
  for (invalid in invalid_input){
    expect_error(blood_group_checker(invalid))
  }
})

test_that('Test Age Checker', {
  valid_input <- c(2, 10, 50, 33, 29, 80)
  invalid_input <- c(1, 99, '50', -1, c(10))
  
  for (valid in valid_input){
    age_checker(valid)
  }
  
  for (invalid in invalid_input){
    expect_error(age_checker(invalid))
  }
})

test_that('Test Tier Checker', {
  valid_input <- c('A', 'B')
  invalid_input <- c('a', 'AB', 'Z')
  
  for (valid in valid_input){
    tier_checker(valid)
  }
  
  for (invalid in invalid_input){
    expect_error(tier_checker(invalid))
  }
})

test_that('Test RRI Checker', {
  valid_input <- c('R1', 'R2', 'R3', 'R4')
  invalid_input <- c('r1', 'R1R2', 'Z1')
  
  for (valid in valid_input){
    rri_checker(valid)
  }
  
  for (invalid in invalid_input){
    expect_error(rri_checker(invalid))
  }
})

test_that('Test Candidate Dataframe Checker', {
  valid_input <- list(
    data.frame(
      ID = c(1),
      bg = c('A'),
      A1 = c(1),
      A2 = c(1),
      B1 = c(1),
      B2 = c(1),
      DR1 = c(1),
      DR2 = c(1),
      age = c(1),
      dialysis = c(1),
      cPRA = c(1),
      urgent = c(1)
    )
  )
  
  invalid_input <- list(
    data.frame(
      ID = c(1),
      bg = c('A'),
      A1 = c(1),
      A2 = c(1),
      B1 = c(1),
      B2 = c(1),
      DR1 = c(1),
      DR2 = c(1),
      age = c(1),
      dialysis = c(1),
      cPRA = c(1)
    ),
    data.frame(
      ID = c(1),
      bg = c('A'),
      A1 = c(1),
      A2 = c(1),
      B1 = c(1),
      B2 = c(1),
      DR1 = c(1),
      DR2 = c(1),
      age = c(1),
      dialysis = c(1),
      cPRA = c(1),
      urgent = c(1),
      random_column = c(TRUE)
    )
  )
  
  mockr::with_mock(
    blood_group_checker = function(var) TRUE,
    age_checker = function(var) TRUE, 
    {
      for (valid in valid_input){
        candidate_dataframe_check(valid)
      }
    
      for (invalid in invalid_input){
        expect_error(candidate_dataframe_check(invalid))
      }
    })
})

test_that('Test UK Candidate Dataframe Checker', {
  valid_input <- list(
    data.frame(
      ID = c(1),
      bg = c('A'),
      A1 = c(1),
      A2 = c(1),
      B1 = c(1),
      B2 = c(1),
      DR1 = c(1),
      DR2 = c(1),
      age = c(1),
      dialysis = c(1),
      cPRA = c(1),
      Tier = c(1),
      MS = c(1),
      RRI = c(1),
      urgent = c(1)
    )
  )
  
  invalid_input <- list(
    data.frame(
      ID = c(1),
      bg = c('A'),
      A1 = c(1),
      A2 = c(1),
      B1 = c(1),
      B2 = c(1),
      DR1 = c(1),
      DR2 = c(1),
      age = c(1),
      dialysis = c(1),
      cPRA = c(1),
      Tier = c(1),
      MS = c(1),
      RRI = c(1)
    ),
    data.frame(
      ID = c(1),
      bg = c('A'),
      A1 = c(1),
      A2 = c(1),
      B1 = c(1),
      B2 = c(1),
      DR1 = c(1),
      DR2 = c(1),
      age = c(1),
      dialysis = c(1),
      cPRA = c(1),
      Tier = c(1),
      MS = c(1),
      RRI = c(1),
      urgent = c(1),
      random_column = c(TRUE)
    )
  )
  
  mockr::with_mock(
    blood_group_checker = function(var) TRUE,
    tier_checker = function(var) TRUE,
    age_checker = function(var) TRUE, 
    rri_checker = function(var) TRUE, 
    {
      for (valid in valid_input){
        expect_equal(uk_candidate_dataframe_check(valid), TRUE)
      }
    
      for (invalid in invalid_input){
        expect_error(uk_candidate_dataframe_check(invalid))
      }
  })
})

test_that("Test cp function", {
    q2_ <- 60
    q3_ <- 100
    cPRA1_ <- 50
    cPRA2_ <- 85

    candids <- data.frame(
        ID = c(1, 1, 1, 1, 1, 1),
        bg = c('A', 'A', 'A', 'A', 'A', 'A'),
        A1 = c(1, 1, 1, 1, 1, 1),
        A2 = c(1, 1, 1, 1, 1, 1),
        B1 = c(1, 1, 1, 1, 1, 1),
        B2 = c(1, 1, 1, 1, 1, 1),
        DR1 = c(1, 1, 1, 1, 1, 1),
        DR2 = c(1, 1, 1, 1, 1, 1),
        age = c(1, 1, 1, 1, 1, 1),
        dialysis = c(1, 1, 101, 1, 61, 1),
        cPRA = c(1, 86, 1, 51, 1, 1),
        urgent = c(1, 0, 0, 0, 0, 0)
    )

    results <- factor(
        list(1, 2, 2, 3, 3, 4), 
        levels = 1:4,
        labels = c('Red', 'Orange', 'Yellow', 'Green')
    )

    expect_equal(
        cp(
            data = candids,
            q2 = q2_,
            q3 = q3_,
            cPRA1 = cPRA1_,
            cPRA2 = cPRA2_
        )$cp,
        results
    )
})