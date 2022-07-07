test_that(" Mismatch Probability (MMP) from ETKAS", {
})

test_that("mmHLA points from ET's algorithm", {

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,1))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 333.33)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 500,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 500)

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,2))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 266.67)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 500,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 500)

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,3))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 200)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 500,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 500)

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,6))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 0)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 500)[['ptsHLA']], 500)
  })


test_that("ET points for time on dialysis (in months)", {
  expect_equal(et_dialysis(dialysis = 0, month = 2.78), 0)
  expect_equal(et_dialysis(dialysis = 1, month = 2.78), 2.78)
  expect_equal(et_dialysis(dialysis = 10, month = 2.78), 27.8)
  expect_equal(et_dialysis(dialysis = 100, month = 2.78), 278)
  })

test_that("et algorithm", {

  cand_test <- data.frame(
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
    cPRA = c(86, 86, 51, 51, 0, 0)
    , urgent = c(0, 0, 0, 0, 0, 0)
  )

  result1 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~HI, ~pointsET, ~SP, ~AM,
    '21', 'O', '1', '2', '15', '51', '13', '14', 0, 1, 2, 3, 65, 65, 0, 0, FALSE, 0, 1, 0,
    '2', 'O', '1', '26', '15', '27', '7', '8', 1, 1, 2, 4, 55, 65, 0, 86, TRUE, 231, 0, 1
   )
  setDT(result1)

  result2 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~HI, ~pointsET, ~SP, ~AM,
    #'21', 'O', '1', '2', '15', '51', '13', '14', 0, 1, 2, 3, 65, 65, 0, 0, FALSE, 0, 1, 0,
    '20', 'AB', '3', '32', '18', '27', '7', '7', 2, 2, 2, 6, 30, 40, 0, 0, FALSE, 97, 0, 0
  )
  setDT(result2)

  result3 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~HI, ~pointsET, ~SP, ~AM,
    '2', 'O', '1', '26', '15', '27', '7', '8', 1, 2, 2, 5, 55, 60, 0, 86, TRUE, 164, 0, 1,
    '1', 'O', '2', '11', '13', '40', '4', '7', 1, 2, 2, 5, 58, 60, 0, 86, TRUE, 155, 0, 1
  )
  setDT(result3)

  result4 <- tibble::tribble(
    ~ID, ~bg, ~A1, ~A2, ~B1, ~B2, ~DR1, ~DR2, ~mmA, ~mmB, ~mmDR, ~mmHLA, ~age, ~donor_age, ~dialysis, ~cPRA, ~HI, ~pointsET, ~SP, ~AM,
    #'2', 'O', '1', '26', '15', '27', '7', '8', 1, 2, 2, 5, 55, 60, 0, 86, TRUE, 164, 0, 1,
    '20', 'AB', '3', '32', '18', '27', '7', '7', 2, 2, 2, 6, 30, 60, 0, 0, FALSE, 97, 0, 0
  )
  setDT(result4)

  expect_equal(et(iso = T
                  , dABO = "O"
                  , dA = c("1","2")
                  , dB = c("15","44")
                  , dDR = c("1","4")
                  , donor.age = 65
                  , data = cand_test
                  , month = 2.78
                  , mm0 = 400
                  , mm1 = 333.33
                  , mm2 = 266.67
                  , mm3 = 200
                  , mm4 = 133.33
                  , mm5 = 66.67
                  , mm6 = 0
                  , df.abs = cabs
                  , hlaA = hlaApt
                  , hlaB = hlaBpt
                  , hlaDR = hlaDRpt
                  , abo_freq = ABOpt
                  , n = 2), result1)
  expect_equal(et(iso = F
                  , dABO = "A"
                  , dA = c("1","2")
                  , dB = c("15","44")
                  , dDR = c("1","4")
                  , donor.age = 40
                  , data = cand_test
                  , month = 2
                  , mm0 = 400
                  , mm1 = 333.33
                  , mm2 = 266.67
                  , mm3 = 200
                  , mm4 = 133.33
                  , mm5 = 66.67
                  , mm6 = 0
                  , df.abs = cabs
                  , hlaA = hlaApt
                  , hlaB = hlaBpt
                  , hlaDR = hlaDRpt
                  , abo_freq = ABOpt
                  , n = 2), result2)
  expect_equal(et(iso = F
                  , dABO = "O"
                  , dA = c("1","2")
                  , dB = c("7","28")
                  , dDR = c("1","14")
                  , donor.age = 60
                  , data = cand_test
                  , month = 4
                  , mm0 = 400
                  , mm1 = 333.33
                  , mm2 = 266.67
                  , mm3 = 200
                  , mm4 = 133.33
                  , mm5 = 66.67
                  , mm6 = 0
                  , df.abs = cabs
                  , hlaA = hlaApt
                  , hlaB = hlaBpt
                  , hlaDR = hlaDRpt
                  , abo_freq = ABOpt
                  , n = 2), result3)
  expect_equal(et(iso = T
                  , dABO = "AB"
                  , dA = c("1","2")
                  , dB = c("7","28")
                  , dDR = c("1","14")
                  , donor.age = 60
                  , data = cand_test
                  , month = 3
                  , mm0 = 400
                  , mm1 = 333.33
                  , mm2 = 266.67
                  , mm3 = 200
                  , mm4 = 133.33
                  , mm5 = 66.67
                  , mm6 = 0
                  , df.abs = cabs
                  , hlaA = hlaApt
                  , hlaB = hlaBpt
                  , hlaDR = hlaDRpt
                  , abo_freq = ABOpt
                  , n = 2), result4)
})

xmatch(dA = c("1","2")
         , dB = c("15","44")
         , dDR = c("1","4")
         , df.abs = cabs)
