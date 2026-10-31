test_that("data from the example project match the stored copy", {
  skip_without_key()
  load(test_path("..", "test_data.rda"))
  cm = getDataCM(climmob_key(), "chocolates",
                 pivot.wider = TRUE, tidynames = TRUE)
  expect_true(all(cm == chocolates, na.rm = TRUE))
})

test_that("an invalid key gives a clear error", {
  skip_on_cran()
  skip_if_offline("climmob.net")
  expect_error(getDataCM("1234", "x", "y"))
})

test_that("request bodies are valid JSON even with quotes in names", {
  b = ClimMobTools:::.request_body('my "project"', "user")
  parsed = jsonlite::fromJSON(b)
  expect_equal(parsed$project_cod, 'my "project"')
  expect_equal(parsed$user_owner, "user")
})
