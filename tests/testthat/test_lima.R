test_that("Test lima algorithm", {
    iso = TRUE
    dABO = "O"
    dA = c("1","2")
    dB = c("15","44")
    dDR = c("1","4")
    donor.age = 60
    df.abs = cabs
    n = 2
    q2 = 60
    q3 = 100
    cPRA1 = 50
    cPRA2 = 85

    candidates <- data.frame(
        ID = c('1', '2', '3', '4', '5', '6'),
        bg = c('O', 'O', 'A', 'B', 'AB', 'O'),
        A1 = c('2', '2', '2', '2', '2', '2'),
        A2 = c('29', '33', '33', '33', '33', '33'),
        B1 = c('44', '15', '15', '15', '15', '15'),
        B2 = c('44', '27', '27', '27', '27', '27'),
        DR1 = c('4', '11', '11', '11', '11', '11'),
        DR2 = c('12', '7', '7', '7', '7', '7'),
        age = c(58, 55, 50, 65, 65, 65),
        dialysis = c(0, 0, 0, 0, 0, 0),
        cPRA = c(86, 86, 51, 51, 51, 51),
        urgent = c(0, 0, 0, 0, 0, 0)
    )

    results <- data.frame(
        ID = c('1', '2'),
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
        donor_age = c(60, 60),
        dialysis = c(0, 0),
        cPRA = c(86, 86),
        HI = c(TRUE, TRUE),
        cp = factor( list(2, 2), levels = 1:4, labels = c('Red', 'Orange', 'Yellow', 'Green') ),
        SP = c(0, 0)
    )

    expect_equal(
        as.data.frame(
            lima(
                iso = iso,
                dABO = dABO,
                dA = dA,
                dB = dB,
                dDR = dDR,
                donor.age = donor.age,
                df.abs = cabs,
                data = candidates,
                n = n,
                q2 = q2,
                q3 = q3,
                cPRA1 = cPRA1,
                cPRA2 = cPRA2
            )
        ),
        results
    )

    expect_error(lima(q2 = env$q.minimum-1),
                 'q2.*')
    expect_error(lima(q2 = env$q.maximum+1),
                 'q2.*')
    expect_error(lima(q3 = env$q.minimum-1),
                 'q2.*')
    expect_error(lima(q3 = env$q.maximum+1),
                 'q2.*')
})
