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

  candidates1 <- data.frame(
    ID = c('1', '2', '3', '4', '5', '6'),
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

  candidates2 <- data.frame(
    ID = c('1', '2', '3', '4', '5', '6'),
    bg = c('O', 'O', 'A', 'B', 'AB', 'O'),
    A1 = c('2', '1', '23', '24', '3', '1'),
    A2 = c('11', '26', '33', '66', '32', '2'),
    B1 = c('13', '15', '8', '35', '18', '15'),
    B2 = c('40', '27', '58', '57', '27', '51'),
    DR1 = c('4', '7', '8', '11', '7', '13'),
    DR2 = c('7', '8', '11', '13', '7', '14'),
    age = c(58, 55, 50, 65, 65, 65),
    dialysis = c(0, 0, 65, 0, 0, 101),
    cPRA = c(86, 86, 0, 51, 49, 0),
    urgent = c(0, 0, 0, 0, 0, 0)
  )

  results1 <- data.frame(
    ID = c('6', '1', '2'),
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

  results2 <- data.frame(
    ID = c('6', '1', '2'),
    bg = c('O', 'O', 'O'),
    A1 = c('1', '2', '1'),
    A2 = c('2', '11', '26'),
    B1 = c('15', '13', '15'),
    B2 = c('51', '40', '27'),
    DR1 = c('13', '4', '7'),
    DR2 = c('14', '7', '8'),
    mmA = c(0, 1, 1),
    mmB = c(1, 2, 1),
    mmDR = c(2, 1, 2),
    mmHLA = c(3, 4, 4),
    age = c(65, 58, 55),
    donor_age = c(60, 60, 60),
    dialysis = c(101, 0, 0),
    cPRA = c(0, 86, 86),
    HI = c(FALSE, TRUE, TRUE),
    ptsEQM = c(40, 10, 10),
    SP = c(0,0,0),
    AM = c(0,1,1),
    urgent = c(0,0,0)
  )

  results3 <- data.frame(
    ID = c('1', '2', '6'),
    bg = c('O', 'O', 'O'),
    A1 = c('2', '2', '2'),
    A2 = c('29', '33', '33'),
    B1 = c('44', '15', '15'),
    B2 = c('44', '27', '27'),
    DR1 = c('4', '11', '11'),
    DR2 = c('12', '7', '7'),
    mmA = c(1, 1, 1),
    mmB = c(1, 1, 1),
    mmDR = c(1, 2, 2),
    mmHLA = c(3, 4, 4),
    age = c(58, 55, 65),
    donor_age = c(60, 60, 60),
    dialysis = c(0, 0, 101),
    cPRA = c(86, 86, 51),
    HI = c(TRUE, TRUE, FALSE),
    ptsEQM = c(50, 50, 90),
    SP = c(0,0,0),
    AM = c(1,1,0),
    urgent = c(0,0,0)
  )

  results4 <- data.frame(
    ID = c('6', '1', '2'),
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
    donor_age = c(70, 70, 70),
    dialysis = c(101, 0, 0),
    cPRA = c(51, 86, 86),
    HI = c(FALSE, TRUE, TRUE),
    ptsEQM = c(50, -140, -140),
    SP = c(1,0,0),
    AM = c(0,1,1),
    urgent = c(0,0,0)
  )

  expect_equal(ignore_attr = TRUE,
               eqm(iso = iso,
                  dABO = dABO,
                  dA = dA,
                  dB = dB,
                  dDR = dDR,
                  donor.age = donor.age,
                  df.abs = cabs,
                  data = candidates1,
                  n = 6,
                  q2 = q2,
                  q3 = q3,
                  uj.matx = uj_matx(max.val = 100, ratio.util = 0.1, ratio.just = 0.1),
                  SP = FALSE, AM = FALSE, mm000 = FALSE
              ),
              results1
              )

  expect_equal(ignore_attr = TRUE,
               eqm(iso = iso,
                   dABO = dABO,
                   dA = dA,
                   dB = dB,
                   dDR = dDR,
                   donor.age = donor.age,
                   df.abs = cabs,
                   data = candidates2,
                   n = 6,
                   q2 = q2,
                   q3 = q3,
                   uj.matx = uj_matx(max.val = 100, ratio.util = 0.5, ratio.just = 0.1),
                   SP = FALSE, AM = FALSE, mm000 = FALSE
               ),
               results2
  )

  expect_equal(ignore_attr = TRUE,
               eqm(iso = iso,
                   dABO = dABO,
                   dA = dA,
                   dB = dB,
                   dDR = dDR,
                   donor.age = donor.age,
                   df.abs = cabs,
                   data = candidates1,
                   n = 6,
                   q2 = q2,
                   q3 = q3,
                   uj.matx = uj_matx(max.val = 100, ratio.util = 0.1, ratio.just = 0.1),
                   SP = TRUE, AM = TRUE, mm000 = TRUE
               ),
               results3
               )

  expect_equal(ignore_attr = TRUE,
               eqm(iso = iso,
                   dABO = dABO,
                   dA = dA,
                   dB = dB,
                   dDR = dDR,
                   donor.age = 70,
                   df.abs = cabs,
                   data = candidates1,
                   n = 6,
                   q2 = q2,
                   q3 = q3,
                   uj.matx = uj_matx(max.val = 100, ratio.util = 0.5, ratio.just = 0.1),
                   SP = TRUE, AM = FALSE, mm000 = FALSE
               ),
               results4
               )

})
