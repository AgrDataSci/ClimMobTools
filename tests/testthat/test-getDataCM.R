context("test-getDataCM")

load("../test_data.rda")

key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

test_that("pivot.wider", {
  
  cm <- getDataCM(key,
                  "chocolates",
                  pivot.wider = TRUE,
                  tidynames = TRUE)
  
  cm <- all(cm == chocolates, na.rm = TRUE)
  
  expect_true(cm)

})

test_that("no API key", {
  
  expect_error(getDataCM("1234", "x"))
  
})


test_that("error no data", {
  expect_error(
    getDataCM(key,
              "empty")
  )
}
)



