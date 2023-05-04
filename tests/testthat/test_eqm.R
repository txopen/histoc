test_that("test uj_matx", {

  mat_test <- matrix(c(100, 90, 80, 70, 60, 50,
                       90, 80, 70, 60, 50, 40,
                       80, 70, 60, 50, 40, 30,
                       70, 60, 50, 40, 30, 20,
                       60, 50, 40, 30, 20, 10,
                       50, 40, 30, 20, 10, 0),
                     nrow = 6,
                     byrow = T)

  expect_equal(uj_matx(max.val = 100, ratio.util = 0.1, ratio.just = 0.1),
               mat_test)

  expect_error(uj_matx(max.val = 0))
  expect_error(uj_matx(max.val = 'A'))

  expect_error(uj_matx(ratio.util = 0.6))
  expect_error(uj_matx(ratio.util = 0))

  expect_error(uj_matx(just.util = 0.6))
  expect_error(uj_matx(just.util = 0))

})

test_that("test eqm", {

  iso = TRUE
  dABO = "O"
  dA = c("1","2")
  dB = c("15","44")
  dDR = c("1","4")
  donor.age = 60
  n = 2
  q2 = 60
  q3 = 100
  cPRA1 = 50
  cPRA2 = 85

  candidates <- data.frame(
    ID = c('K1', 'K2', 'K3', 'K4', 'K5', 'K6'),
    bg = c('O', 'O', 'A', 'B', 'AB', 'O'),
    A1 = c('2', '2', '2', '2', '2', '2'),
    A2 = c('29', '33', '33', '33', '33', '33'),
    B1 = c('44', '15', '15', '15', '15', '15'),
    B2 = c('44', '27', '27', '27', '27', '27'),
    DR1 = c('4', '11', '11', '11', '11', '11'),
    DR2 = c('12', '7', '7', '7', '7', '7'),
    age = c(58, 55, 50, 65, 65, 65),
    dialysis = c(0, 0, 0, 0, 0, 101),
    cPRA = c(86, 86, 51, 51, 49, 51),
    urgent = c(0, 0, 0, 0, 0, 0)
  )

  results <- data.frame(
    ID = c('K6', 'K1', 'K2'),
    bg = c('O', 'O', 'O'),
    A1 = c('2', '2', '2'),
    A2 = c('33', '29', '33'),
    B1 = c('15', '44', '15'),
    B2 = c('27', '44', '27'),
    DR1 = c('11', '4', '11'),
    DR2 = c('7', '12', '7'),
    mmA = c(1, 1, 1),
    mmB = c(1, 1, 1),
    mmDR = c(2, 1, 2),
    mmHLA = c(4, 3, 4),
    age = c(65, 58, 55),
    donor_age = c(60, 60, 60),
    dialysis = c(101, 0, 0),
    cPRA = c(51, 86, 86),
    HI = c(FALSE, TRUE, TRUE),
    ptsEQM = c(90, 50, 50),
    SP = c(0,0,0),
    AM = c(0,1,1),
    urgent = c(0,0,0)
  )

  library(histoc)

  expect_equal(ignore_attr = TRUE,
    eqm(iso = iso,
        dABO = dABO,
        dA = dA,
        dB = dB,
        dDR = dDR,
        donor.age = donor.age,
        df.abs = cabs,
        data = candidates,
        n = 6,
        q2 = q2,
        q3 = q3,
        uj.matx = uj_matx(max.val = 100, ratio.util = 0.1, ratio.just = 0.1)
    ),
    results
  )

})
