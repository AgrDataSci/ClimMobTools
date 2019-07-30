context("test-rainfall")

library("ClimMobTools")
#load("tests/clim.rda")
load("../clim.rda")


days <- dimnames(rain)[[2]]

d <- as.Date(rep(days[2:3], each=5), format = "%Y-%m-%d")

MLDS <- c(rep(9, 2), rep(10, 8))
MLWS <- c(rep(1, 2), rep(0, 8))

test_that("dry equal", {
  
  r <- rainfall(object = rain, 
                day.one = d,
                span = 10)
  
  ds <- all.equal(MLDS, r$MLDS)
  
  expect_equal(ds, TRUE)
})

test_that("moist equal", {
  
  r <- rainfall(object = rain, 
                day.one = d,
                span = 10)
  
  ws <- all.equal(MLWS, r$MLWS)
  
  expect_equal(ws, TRUE)
})




