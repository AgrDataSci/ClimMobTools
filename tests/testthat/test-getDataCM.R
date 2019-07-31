context("test-getProjectsCM")

library("ClimMobTools")

key <- "d39a3c66-5822-4930-a9d4-50e7da041e77"

p <- getDataCM(key,"colours")
p <- !is.null(p)


test_that("no API key", {
  
  expect_error(getDataCM("1234", "x"))
  
})

test_that("api call works", {
  
  expect_equal(p, TRUE)
  
})


test_that("pivot.wider", {
  
  cm <- getDataCM(key,
                  "breadwheat",
                  pivot.wider = TRUE,
                  tidynames = TRUE)
  
  wider <- ncol(cm) > 10
  
  expect_equal(wider, TRUE)

})

test_that("error no data", {
  expect_error(
    getDataCM("d39a3c66-5822-4930-a9d4-50e7da041e77",
              "wageningen")
  )
}
)



