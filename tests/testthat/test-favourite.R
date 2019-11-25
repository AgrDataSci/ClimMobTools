context("test-favourite")

library("ClimMobTools")
#load("tests/clim.rda")
load("../clim.rda")

test_that("accepts data = null", {
  
  fav <- favourite(items = triadic[,c(1:3)],
                   input = triadic[,c(4:5)])
  
  
  expect_equal(is.data.frame(fav), TRUE)

})

test_that("accepts indexed items and input", {
  
  fav <- favourite(data = triadic,
                   items = c(1:3),
                   input = c(4:5),
                   reorder = FALSE)
  
  
  expect_equal(is.data.frame(fav), TRUE)
  
})


test_that("accepts named items and input", {
  
  fav <- favourite(data = triadic,
                   items = c("item_A","item_B","item_C"),
                   input = c("best","worst"))
  
  
  expect_equal(is.data.frame(fav), TRUE)
  
})


test_that("accepts 4 or more comparisons", {
  
  fav <- favourite(data = tetra,
                   items = c(1:5),
                   input = c(6:10))
  
  expect_equal(is.data.frame(fav), TRUE)
  
})