env <- new.env()

env$adulthood.age <- 18
env$person.maximum.age <- 99
env$person.minimum.age <- 1
env$minimum.age.difference.points <- 1
env$maximum.age.difference.points <- 20
env$cPRA.minimum <- 0
env$cPRA.maximum <- 100
env$dialysis.minimum <- 0
env$dialysis.maximum <- 999
env$dirj.minimum <- 0
env$dirj.maximum <- 1000
env$percentage.minimum <- 0
env$percentage.maximum <- 100
env$q.minimum <- 0
env$q.maximum <- 120
env$pt.points.minimum <- 0
env$pt.points.maximum <- 20
env$points.dialysis.minimum <- 0
env$points.dialysis.maximum <- 2
env$points.age.minimum <- 0
env$points.age.maximum <- 20
env$month.points.minimum <- 0
env$month.points.maximum <- 10


env$valid.dris <- c('D1','D2','D3','D4')
env$valid.tiers <- c('A', 'B')
env$valid.rris <- c('R1', 'R2', 'R3', 'R4')
env$valid.blood.groups <- c('O', 'A', 'B', 'AB')
env$valid.urgent <- c(0, 1)
env$color.priority.labels <- c('Red', 'Orange', 'Yellow', 'Green')