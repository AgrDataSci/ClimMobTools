context("test-ETo")

library("ClimMobTools")
#load("tests/clim.rda")
load("../clim.rda")


days <- dimnames(rain)[[2]]

d <- as.Date(rep(days[2:3], each=5), format = "%Y-%m-%d")

e <- c(4.752054,4.897989,4.659525,3.947859,4.535946,4.231035,4.535325,4.390011,4.412367,4.124223)


test_that("equal", {
  
  ev <- ETo(temp,
            day.one = d, 
            span = 10,
            lat = rep(0, 10))
  
  ev <- all.equal(e, ev[[1]])
  
  expect_equal(ev, TRUE)
})

