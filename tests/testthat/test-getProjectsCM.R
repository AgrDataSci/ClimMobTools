test_that("an invalid key gives an error", {
  skip_on_cran()
  skip_if_offline("climmob.net")
  expect_error(getProjectsCM("1234"))
})

test_that("projects are listed with a valid key", {
  skip_without_key()
  p = getProjectsCM(climmob_key())
  expect_false(is.null(p))
})
