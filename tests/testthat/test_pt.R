test_that("Test pts_age function", {
    valid.donor.ages <- c(50, 61, 61, 39, 39)
    valid.candidate.ages <- c(40, 55, 54, 55, 56)
    valid.age.difference.points <- c(4, 4, 4, 4, 4)
    valid.results <- c(4, 4, 0, 4, 0)


    for (i in 1:length(valid.donor.ages)){
        expect_equal(
            pts_age(
                donor.age <- valid.donor.ages[i],
                candidate.age <- valid.candidate.ages[i],
                age.difference.points <- valid.age.difference.points[i]
            ),
            valid.results[i]
        )
    }  

    invalid.donor.ages <- c('50', 61, 61, env$adulthood.age - 1, 39, 39, env$person.maximum.age, 39, 39)
    invalid.candidate.ages <- c(40, '55', 54, 55, env$adulthood.age - 1, 39, 39, env$person.maximum.age, 39)
    invalid.age.difference.points <- c(4, 4, '4', 4, 4, env$minimum.age.difference.points - 1, 4, 4, env$maximum.age.difference.points + 1)

    for (i in 1:length(valid.donor.ages)){
        expect_error(
            pts_age(
                donor.age <- invalid.donor.ages[i],
                candidate.age <- invalid.candidate.ages[i],
                age.difference.points <- invalid.age.difference.points[i]
            )
        )
    }  
})

test_that("Test pts_PRA function", {
    valid.cPRA <- c(1, 50, 51, 80, 81)
    valid.points.50 <- c(2, 2, 2, 2, 2)
    valid.points.80 <- c(4, 4, 4, 4, 4)
    valid.results <- c(0, 2, 2, 4, 4)


    for (i in 1:length(valid.cPRA)){
        expect_equal(
            pts_PRA(
                cPRA <- valid.cPRA[i],
                points.50 <- valid.points.80[i], # ????????????
                points.80 <- valid.points.50[i]  # ????????????
            ),
            valid.results[i]
        )
    }  
    
    invalid.cPRA <- c('1', -1, 101, 4, 4, 4)
    invalid.points.50 <- c(4, 4, 4, 4, 4, 4, '1', -1, 101)
    invalid.points.80 <- c(4, 4, 4, '1', -1, 101, 4, 4, 4)

    for (i in 1:length(invalid.cPRA)){
        expect_error(
            pts_PRA(
                cPRA <- invalid.cPRA[i],
                points.50 <- invalid.points.50[i],
                points.80 <- invalid.points.80[i]
            )
        )
    }  
})


test_that("Test pt algorithm", {
    iso = TRUE
    dABO = "O"
    dA = c("1","2")
    dB = c("15","44")
    dDR = c("1","4")
    donor.age = 65
    df.abs = cabs
    points.80 = 8
    points.50 = 4
    points.dialysis = 0.1
    points.age = 4
    n = 2

    candidates_test <- data.frame(
        ID = c(1, 2, 3, 4, 5, 6),
        bg = c('O', 'O', 'A', 'B', 'AB', 'O'),
        A1 = c('2', '2', '2', '2', '2', '2'),
        A2 = c('29', '33', '33', '33', '33', '33'),
        B1 = c('44', '15', '15', '15', '15', '15'),
        B2 = c('44', '27', '27', '27', '27', '27'),
        DR1 = c('4', '11', '11', '11', '11', '11'),
        DR2 = c('12', '7', '7', '7', '7', '7'),
        age = c(58, 55, 50, 65, 65, 65),
        dialysis = c(104, 103, 0, 0, 0, 0),
        cPRA = c(0, 0, 0, 0, 0, 0),
        urgent = c(0, 0, 0, 0, 0, 0)
    )

#   ID bg A1 A2 B1 B2 DR1 DR2 mmA mmB mmDR mmHLA age donor_age dialysis cPRA    HI ptsPT SP ptsHLA ptsPRA ptsage ptsdial
# 1: 1  O  2 29 44 44   4  12   1   1    1     3  58        65      104    0 FALSE  16.4  3      2      0      4    10.4
# 2: 2  O  2 33 15 27  11   7   1   1    2     4  55        65      103    0 FALSE  15.3  3      1      0      4    10.3

    results <- data.frame(
        ID = c(1, 2),
        bg = c('O', 'O'),
        A1 = c('2', '2'),
        A2 = c('29', '33'),
        B1 = c('44', '15'),
        B2 = c('44', '27'),
        DR1 = c('4', '11'),
        DR2 = c('12', '7'),
        mmA = c(1, 1),
        mmB = c(1, 1),
        mmDR = c(1, 2),
        mmHLA = c(3, 4),
        age = c(58, 55),
        donor_age = c(65, 65),
        dialysis = c(104, 103),
        cPRA = c(0, 0),
        HI = c(FALSE, FALSE),
        ptsPT = c(16.4, 15.3),
        SP = c(3, 3),
        ptsHLA = c(2, 1),
        ptsPRA = c(0, 0),
        ptsage = c(4, 4),
        ptsdial = c(10.4, 10.3)
    )
    
    expect_equal(
        as.data.frame(
            pt(
                iso = TRUE
                , dABO = "O"
                , dA = c("1","2")
                , dB = c("15","44")
                , dDR = c("1","4")
                , donor.age = 65
                , df.abs = cabs
                , data = candidates_test
                , points.80 = 8
                , points.50 = 4
                , points.dialysis = 0.1
                , points.age = 4
                , n = 2
            )
        ),
        results
    )
})
