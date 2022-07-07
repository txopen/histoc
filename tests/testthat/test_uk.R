test_that("Donor-recipient Risk Index Combination", {

  cand_uk_test <- data.frame(
    ID = c('1','2','11','19','20','21'),
    bg = c('O', 'O', 'A', 'B', 'AB', 'O'),
    A1 = c('2', '1', '23', '24', '3', '1'),
    A2 = c('11', '26', '33', '66', '32', '2'),
    B1 = c('13', '15', '8', '35', '18', '15'),
    B2 = c('40', '27', '58', '57', '27', '51'),
    DR1 = c('4', '7', '8', '11', '7', '13'),
    DR2 = c('7', '8', '11', '13', '7', '14'),
    age = c(58, 55, 50, 65, 30, 65),
    dialysis = c(0, 0, 0, 0, 0, 0),
    cPRA = c(86, 86, 51, 51, 0, 0),
    Tier = c('A','B','B','B','B','B'),
    MS = c(3,5,7,8,1,8),
    RRI = c('R2','R1','R3','R2','R4','R1'),
    urgent = c(0, 0, 0, 0, 0, 0)
  )

  result1 <- cand_uk_test %>%
    dplyr::mutate(ric = c(700,1000,350,700,0,1000))
  result2 <- cand_uk_test %>%
    dplyr::mutate(ric = c(1000,700,500,1000,350,700))
  result3 <- cand_uk_test %>%
    dplyr::mutate(ric = c(500,350,1000,500,700,350))
  result4 <- cand_uk_test %>%
    dplyr::mutate(ric = c(350,0,700,350,1000,0))

  expect_equal(ric(DRI = 'D1',
                   data =cand_uk_test,
                   D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
                   D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
                   D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
                   D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000
                   ), result1)
  expect_equal(ric(DRI = 'D2',
                   data =cand_uk_test,
                   D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
                   D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
                   D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
                   D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000
                   ), result2)
  expect_equal(ric(DRI = 'D3',
                   data =cand_uk_test,
                   D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
                   D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
                   D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
                   D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000
                   ), result3)
  expect_equal(ric(DRI = 'D4',
                   data =cand_uk_test,
                   D1R1 = 1000, D1R2 = 700, D1R3 = 350, D1R4 = 0,
                   D2R1 = 700, D2R2 = 1000, D2R3 = 500, D2R4 = 350,
                   D3R1 = 350, D3R2 = 500, D3R3 = 1000, D3R4 = 700,
                   D4R1 = 0, D4R2 = 350, D4R3 = 700, D4R4 = 1000
                   ), result4)
})

test_that("Donor recipient age difference", {
  expect_equal(age_diff(donor.age = 60,
                        candidate.age = 50), -50)
  expect_equal(age_diff(donor.age = 20,
                        candidate.age = 60), -800)
  expect_equal(age_diff(donor.age = 20,
                        candidate.age = 20), 0)
  expect_equal(age_diff(donor.age = 60,
                        candidate.age = 40), -200)
  expect_equal(age_diff(donor.age = 60,
                        candidate.age = 20), -800)
  expect_equal(age_diff(donor.age = 18,
                        candidate.age = 69), -1300.5)
})

test_that("blood group B match points", {
  expect_equal(b_blood_penalization(dABO = "B",
                       cABO = "O",
                       tier = "B",
                       pts = -1000), 0)
  expect_equal(b_blood_penalization(dABO = "O",
                       cABO = "B",
                       tier = "B",
                       pts = -1000), -1000)
  expect_equal(b_blood_penalization(dABO = "A",
                       cABO = "A",
                       tier = "A",
                       pts = -1000), 0)
  expect_equal(b_blood_penalization(dABO = "O",
                       cABO = "B",
                       tier = "B",
                       pts = -2000), -2000)
  expect_equal(b_blood_penalization(dABO = "O",
                       cABO = "AB",
                       tier = "B",
                       pts = -2000), 0)
})

test_that("test for ABO compatibility on UK transplant", {
  expect_true(abo_uk(dABO = "A",
                      cABO = "A",
                      tier = "B"))
  expect_true(abo_uk(dABO = "A",
                     cABO = "AB",
                     tier = "B"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "O",
                     tier = "B"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "B",
                     tier = "B"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "A",
                     tier = "A"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "AB",
                     tier = "A"))
  expect_true(abo_uk(dABO = "B",
                     cABO = "B",
                     tier = "B"))
  expect_true(abo_uk(dABO = "B",
                     cABO = "B",
                     tier = "A"))
  expect_true(abo_uk(dABO = "AB",
                     cABO = "AB",
                     tier = "A"))
  expect_true(abo_uk(dABO = "AB",
                     cABO = "AB",
                     tier = "B"))

  expect_false(abo_uk(dABO = "A",
                     cABO = "O",
                     tier = "B"))
  expect_false(abo_uk(dABO = "B",
                      cABO = "O",
                      tier = "B"))
  expect_false(abo_uk(dABO = "AB",
                      cABO = "B",
                      tier = "B"))
})

test_that("uk algorithm", {

  cand_uk_test <- data.frame(
    ID = c('1','2','11','19','20','21'),
    bg = c('O', 'O', 'A', 'B', 'AB', 'O'),
    A1 = c('2', '1', '23', '24', '3', '1'),
    A2 = c('11', '26', '33', '66', '32', '2'),
    B1 = c('13', '15', '8', '35', '18', '15'),
    B2 = c('40', '27', '58', '57', '27', '51'),
    DR1 = c('4', '7', '8', '11', '7', '13'),
    DR2 = c('7', '8', '11', '13', '7', '14'),
    age = c(58, 55, 50, 65, 30, 65),
    dialysis = c(0, 0, 0, 0, 0, 0),
    cPRA = c(86, 86, 51, 51, 0, 0),
    Tier = c('A','B','B','B','B','B'),
    MS = c(3,5,7,8,1,8),
    RRI = c('R2','R1','R3','R2','R2','R1'),
    urgent = c(0, 0, 0, 0, 0, 0)
  )

  result1 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~matchability, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~Tier, ~pointsUK,
    '1', 'O', '2', '11', '13', '40', '4', '7', 45.9, 1, 2, 1, 4, 58, 40, 0, 86, 'A', 9999,
    '21', 'O', '1', '2', '15', '51', '13', '14', 637.7, 0, 1, 2, 3, 65, 40, 0, 0, 'B', 1560.6
  )
  setDT(result1)

  result2 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~matchability, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~Tier, ~pointsUK,
    '20', 'AB', '3', '32', '18', '27', '7', '7', 50, 2, 2, 1, 5, 30, 30, 0, 0, 'B', 1025.9,
    '11', 'A', '23', '33', '8', '58', '8', '11', 448.9, 2, 2, 2, 6, 50, 30, 0, 51, 'B', 835.5
  )
  setDT(result2)

  result3 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~matchability, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~Tier, ~pointsUK,
    '1', 'O', '2', '11', '13', '40', '4', '7', 51.8, 1, 2, 2, 5, 58, 60, 0, 86, 'A', 9999,
    '21', 'O', '1', '2', '15', '51', '13', '14', 5647.1, 0, 2, 1, 3, 65, 60, 0, 0, 'B', 5970
  )
  setDT(result3)

  expect_equal(uk(DRI = 'D1',
                  dA = c("1","2"),
                  dB = c("15","44"),
                  dDR = c("1","4"),
                  dABO = "O",
                  donor.age = 40,
                  data = cand_uk_test,
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
                  pts = -1000,
                  df.abs = cabs,
                  n = 2), result1)
  expect_equal(uk(DRI = 'D2',
                  dA = c("11","24"),
                  dB = c("7","44"),
                  dDR = c("7","14"),
                  dABO = "A",
                  donor.age = 30,
                  data = cand_uk_test,
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
                  ptsDial = 2,
                  a1 = 2300,
                  a2 = 1500,
                  b1 = 1500,
                  b2 = 750,
                  b3 = 400,
                  m = 50,
                  nn = 4.5,
                  o = 4.7,
                  mm1 = -100,
                  mm23 = -150,
                  mm46 = -250,
                  pts = -2000,
                  df.abs = cabs,
                  n = 2), result2)
  expect_equal(uk(DRI = 'D4',
                  dA = c("1","2"),
                  dB = c("27","44"),
                  dDR = c("11","14"),
                  dABO = "O",
                  donor.age = 60,
                  data = cand_uk_test,
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
                  ptsDial = 1.9,
                  a1 = 2300,
                  a2 = 1500,
                  b1 = 1200,
                  b2 = 750,
                  b3 = 400,
                  m = 50,
                  nn = 4.5,
                  o = 8.2,
                  mm1 = -400,
                  mm23 = -50,
                  mm46 = -50,
                  pts = -1000,
                  df.abs = cabs,
                  n = 2), result3)
})
