test_that("compatibility ABO", {
  expect_false(abo(cABO = 'A', dABO = 'O', iso = TRUE))
  expect_true(abo(cABO = 'A', dABO = 'O', iso = FALSE))
  expect_false(abo(cABO = 'AB', dABO = 'A', iso = TRUE))
  expect_true(abo(cABO = 'AB', dABO = 'A', iso = FALSE))
  expect_true(abo(cABO = 'AB', dABO = 'AB', iso = TRUE))
})

test_that("computes HLA mismatchs", {
  expect_equal(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07'))[['mmA']], 0)
  expect_equal(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07'))[['mmB']], 2)
  expect_equal(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07'))[['mmDR']], 1)
  expect_equal(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07'))[['mmHLA']], 3)

  expect_error(mmHLA(dA = c(1,2),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07')), "donor.*")
  expect_error(mmHLA(dA = c('01','02'),
                     dB = 1L,
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07')), "donor.*")
  expect_error(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = 100,
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c('04','07')), "donor.*")
  expect_error(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c(1,2),
                     cB = c('03','15'),
                     cDR = c('04','07')), "candidate.*")
  expect_error(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c(1,2),
                     cDR = c('04','07')), "candidate.*")
  expect_error(mmHLA(dA = c('01','02'),
                     dB = c('05','07'),
                     dDR = c('01','04'),
                     cA = c('01','02'),
                     cB = c('03','15'),
                     cDR = c(1,2)), "candidate.*")
})

test_that("virtual crossmatch", {
  xmatch(dA = c('1','2'),
         dB = c('5','7'),
         dDR = c('1','4'),
         df.abs = cabs)$xm %>% .[1] %>%
    expect_equal("NEG")
  xmatch(dA = c('1','2'),
         dB = c('5','7'),
         dDR = c('1','4'),
         df.abs = cabs)$xm %>% .[6] %>%
    expect_equal("POS")
  xmatch(dA = c('1','2'),
         dB = c('5','7'),
         dDR = c('1','4'),
         df.abs = cabs)$xm %>% .[10] %>%
    expect_equal("POS")

  expect_error(xmatch(dA = c(1,2),
                      dB = c('5','7'),
                      dDR = c('1','4'),
                      df.abs = cabs), "xmatch.*")
  expect_error(xmatch(dA = c('1','2'),
                      dB = c(1,2),
                      dDR = c('1','4'),
                      df.abs = cabs), "xmatch.*")
  expect_error(xmatch(dA = c('1','2'),
                      dB = c('5','7'),
                      dDR = c(1,2),
                      df.abs = cabs), "xmatch.*")

})


test_that("Hiperimunized patients", {
  expect_true(hiper(cPRA = 99, cutoff = 85))
  expect_false(hiper(cPRA = 80, cutoff = 85))
  expect_true(hiper(cPRA = 90, cutoff = 80))
  expect_true(hiper(cPRA = 80, cutoff = 80))
  expect_false(hiper(cPRA = 1, cutoff = 85))
  expect_false(hiper(cPRA = 50, cutoff = 60))
})

test_that("Senior program classification", {
  expect_equal(sp(donor.age = 66, candidate.age = 70), 1)
  expect_equal(sp(donor.age = 50, candidate.age = 64), 2)
  expect_equal(sp(donor.age = 66, candidate.age = 64), 3)
  expect_equal(sp(donor.age = 50, candidate.age = 70), 3)
})

test_that("Tx Score (5 year survival probability)", {
  expect_equal(txscore(recipient.age = 40
                       , recipient.race = "White"
                       #, insurance = 0
                       , recipient.causeESRD = "Other"
                       , recipient.dialysis = 60
                       , recipient.diabetes = F
                       , recipient.coronary = F
                       , recipient.albumin = 1.5
                       , recipient.hemoglobin = 10
                       , donor.age = 40
                       , donor.diabetes = "Absence"
                       , donor.ECD = F
                       #, mmHLA = "0"
                       , mmHLA_A = 0
                       , mmHLA_B = 0
                       , mmHLA_DR = 0)$prob5y, 40)
  expect_equal(txscore(recipient.age = 72
                       , recipient.race = "Black"
                       #, insurance = 0
                       , recipient.causeESRD = "Hypertension"
                       , recipient.dialysis = 21
                       , recipient.diabetes = F
                       , recipient.coronary = F
                       , recipient.albumin = 2.7
                       , recipient.hemoglobin = 6.4
                       , donor.age = 63
                       , donor.diabetes = "Unknown"
                       , donor.ECD = T
                       #, mmHLA = "0"
                       , mmHLA_A = 0
                       , mmHLA_B = 1
                       , mmHLA_DR = 2)$prob5y, 57.25)
  expect_gte(txscore(recipient.age = 52
                       , recipient.race = "Black"
                       #, insurance = 0
                       , recipient.causeESRD = "Hypertension"
                       , recipient.dialysis = 0
                       , recipient.diabetes = F
                       , recipient.coronary = F
                       , recipient.albumin = 1
                       , recipient.hemoglobin = 10
                       , donor.age = 70
                       , donor.diabetes = "Unknown"
                       , donor.ECD = F
                       #, mmHLA = "0"
                       , mmHLA_A = 2
                       , mmHLA_B = 1
                       , mmHLA_DR = 0)$prob5y, 0)
  expect_lte(txscore(recipient.age = 52
                     , recipient.race = "Black"
                     #, insurance = 0
                     , recipient.causeESRD = "Hypertension"
                     , recipient.dialysis = 0
                     , recipient.diabetes = F
                     , recipient.coronary = F
                     , recipient.albumin = 1
                     , recipient.hemoglobin = 10
                     , donor.age = 70
                     , donor.diabetes = "Unknown"
                     , donor.ECD = F
                     #, mmHLA = "0"
                     , mmHLA_A = 2
                     , mmHLA_B = 1
                     , mmHLA_DR = 0)$prob5y, 100)

  expect_error(txscore(recipient.race = "XXX"), 'Recipient.*')
  expect_error(txscore(recipient.causeESRD = "XXX"), 'Recipient.*')
  expect_error(txscore(recipient.dialysis = 201), 'Recipient.*')
  expect_error(txscore(recipient.dialysis = -201), 'Recipient.*')
  expect_error(txscore(recipient.diabetes = 1), 'Recipient.*')
  expect_error(txscore(recipient.diabetes = 'X'), 'Recipient.*')
  expect_error(txscore(recipient.coronary = 'X'), 'Recipient.*')
  expect_error(txscore(recipient.coronary = 0), 'Recipient.*')
  expect_error(txscore(recipient.albumin = 0), 'Recipient.*')
  expect_error(txscore(recipient.albumin = 6), 'Recipient.*')
  expect_error(txscore(recipient.hemoglobin = 2), 'Recipient.*')
  expect_error(txscore(recipient.hemoglobin = 21), 'Recipient.*')
  expect_error(txscore(donor.diabetes = 0), 'Donor.*')
  expect_error(txscore(donor.ECD = 0), 'Donor.*')
  expect_error(txscore(mmHLA_A = 1.5), 'Number.*')
  expect_error(txscore(mmHLA_B = 1.5), 'Number.*')
  expect_error(txscore(mmHLA_DR = 1.5), 'Number.*')


  })
