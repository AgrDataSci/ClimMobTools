context("test-build_rankings")

library("ClimMobTools")
#load("tests/clim.rda")
load("../clim.rda")



test_that("triadic ok", {
  
  R <- build_rankings(data = triadic,
                      items = c(1:3),
                      input = c(4:5), 
                      group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  expect_equal(!any(R == triadic_true), FALSE)
  
})


test_that("triadic with local ok", {
  
  R <- build_rankings(data = triadic,
                      items = c(1:3),
                      input = c(4:5), 
                      additional.rank = triadic[,6:8],
                      group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  expect_equal(!any(R == triadic_true_local), FALSE)
  
})


test_that("tretra ok", {
  
  R <- build_rankings(data = NULL,
                      items = tetra[,c(1:5)],
                      input = tetra[,c(6:10)], 
                      group = TRUE)
  
  R <- R[1:length(R),, as.grouped_rankings = FALSE]
  
  expect_equal(!any(R == tetra_true), FALSE)
  
})


test_that("full.output works", {
  
  R <- build_rankings(data = triadic,
                      items = c(1:3),
                      input = c(4:5), 
                      additional.rank = triadic[,6:8],
                      full.output = TRUE)
  
  l <- c(!is.list(R), unlist(lapply(R, is.null)))

  expect_equal(any(l), FALSE)
  
})


