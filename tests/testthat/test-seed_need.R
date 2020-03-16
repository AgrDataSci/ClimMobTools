context("test-seed_need")

v <- c(32, 8, 2, 8)

test_that("returns a valid object", {
  s <- seed_need(nobservers = 10,
                 ncomp = 3,
                 nitems = 4,
                 nseeds = .2,
                 unit = "kg")
  
  s <- s$quant
  
  s <- all(s == v)
  
  expect_true(s)
  
})

