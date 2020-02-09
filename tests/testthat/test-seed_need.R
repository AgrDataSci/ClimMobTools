context("test-seed_need")

test_that("returns a valid object", {
  s <- seed_need(nobservers = 400,
                 ncomp = 3,
                 nitems = 9,
                 nseeds = 100,
                 unit = "unit")
  
  v <- as.vector(apply(s, 1, is.na))
  v <- !any(v)
  
  expect_equal(v, TRUE)
})

