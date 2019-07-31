context("test-randomise")

ni <- 3
no <- 10
nv <- 4
inames <- paste("Var", 1:nv, sep="")

r <- randomise(nitems = ni,
               nobservers = no,
               nvar = nv,
               itemnames = inames)

test_that("returns a valid object", {
  v <- !is.null(r)
  expect_equal(v, TRUE)
})

test_that("returns a balanced sample", {
  
  i <- c(7, 8) %in% summary(as.factor(unlist(r)))
  
  expect_equal(i, c(TRUE, TRUE))
})


test_that("returns an error", {
  expect_error(
    randomise(nitems = ni,
              nobservers = no,
              nvar = nv,
              itemnames = inames[1:3])
  )
})

test_that("missing nobservers", {
  expect_error(
    randomise(nitems = ni,
              nobservers = NULL,
              nvar = nv,
              itemnames = inames)
  )
})

test_that("missing nvar", {
  expect_error(
    randomise(nitems = ni,
              nobservers = no,
              nvar = NULL,
              itemnames = inames)
  )
})

test_that("alias works", {
  r <- randomize(nitems = ni,
                 nobservers = no,
                 nvar = nv,
                 itemnames = inames)
  
  r <- as.vector(apply(r, 1, is.na))
  
  r <- sum(r) == 0
  
  expect_equal(r, TRUE)
  
})

