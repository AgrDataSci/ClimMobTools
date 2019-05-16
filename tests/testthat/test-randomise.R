context("test-randomise")

ni <- 3
no <- 10
nv <- 4
inames <- paste("Var", 1:nv, sep="")

r <- randomise(nitems = ni,
               nobservers = no,
               nvar = nv,
               itemnames = inames)

test_that("randomise returns a dataframe", {
  expect_equal(dim(r), c(10, 3))
})
