context("test-temperature")

library("ClimMobTools")
#load("tests/clim.rda")
load("../clim.rda")


days <- dimnames(rain)[[2]]

d <- as.Date(rep(days[2:3], each=5), format = "%Y-%m-%d")


maxDT <- c(33.5,34.9,32,27.5,32.8,27.4,32.6,31.6,32.3,28.5)
minNT <- c(8.2,9.1,8.2,1.5,5.7,6,5.7,4.6,4.3,2.8)

test_that("day equal", {
  
  x <- temperature(object = temp, 
                   day.one = d,
                   span = 8)
  
  dt <- all.equal(maxDT, x$maxDT)
  
  expect_equal(dt, TRUE)
})

test_that("night equal", {
  
  x <- temperature(object = temp, 
                   day.one = d,
                   span = 8)
  
  nt <- all.equal(minNT, x$minNT)
  
  expect_equal(nt, TRUE)
})




