xy = matrix(c(11.097799, 60.801090,
              NA, NA,
              11.161298, 60.804199,
              11.254428, 60.822457),
            ncol = 2, byrow = TRUE)

test_that("points move but stay within the buffer distance", {
  skip_if_not_installed("sf")
  suppressPackageStartupMessages(library(sf))
  r = rmGeoIdentity(xy, dist = 0.015)
  ok = !is.na(xy[, 1])
  d = sqrt((r$long[ok] - xy[ok, 1])^2 + (r$lat[ok] - xy[ok, 2])^2)
  expect_true(all(d <= 0.015 + 1e-9))
  expect_true(all(d > 0))
  expect_true(all(is.na(r[!ok, ])))
})

test_that("no seed is printed and a given seed does not change the caller's stream", {
  skip_if_not_installed("sf")
  suppressPackageStartupMessages(library(sf))
  expect_silent(rmGeoIdentity(xy))
  set.seed(10)
  before = .Random.seed
  rmGeoIdentity(xy, seed = 99)
  expect_identical(.Random.seed, before)
})
