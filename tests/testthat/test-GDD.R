context("test-GDD")

library("ClimMobTools")
#load("tests/clim.rda")
load("../clim.rda")


days <- dimnames(rain)[[2]]

d <- as.Date(rep(days[2:3], each=5), format = "%Y-%m-%d")


g <- c(5,4,5,11,5,7,5,6,6,8)

test_that("equal", {
  
  dg <- GDD(object = temp,
            day.one = d, 
            degree.days = 45,
            base = 10, 
            span = 12)
  
  
  dg <- all.equal(g, dg[[1]])
  
  expect_equal(dg, TRUE)
})

