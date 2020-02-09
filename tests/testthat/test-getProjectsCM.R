context("test-getProjectsCM")

library("ClimMobTools")

test_that("error", {
  
  expect_error(getProjectsCM("1234"))
  
})

p <- getProjectsCM("d39a3c66-5822-4930-a9d4-50e7da041e77")
p <- !is.null(p)

test_that("api call", {
  
  expect_equal(p, TRUE)
  
})