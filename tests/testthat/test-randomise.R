context("test-randomise")

ni <- 3
no <- 10
nv <- 4
inames <- paste("Var", 1:nv, sep="")

r <- randomise(ncomp = ni,
               nobservers = no,
               nitems = nv,
               itemnames = inames)

test_that("returns a valid object", {
  v <- as.vector(apply(r, 1, is.na))
  v <- !any(v)
  
  expect_true(v)
})

test_that("returns a balanced sample", {
  
  i <- c(7, 8) %in% summary(as.factor(unlist(r)))
  i <- sum(i)
  
  expect_equal(i, 2)
})


test_that("works with more than 3 comp", {
  
  r5 <- randomise(ncomp = 5,
                  nobservers = 20,
                  nitems = 10,
                  itemnames = paste("Var", 1:10, sep=""))
  
  v <- as.vector(apply(r5, 1, is.na))
  v <- !any(v)
  
  expect_true(v)

})



test_that("different lenght in nitems and itemnames", {
  expect_error(
    randomise(ncomp = ni,
              nobservers = no,
              nitems = nv,
              itemnames = inames[1:3])
  )
})

test_that("missing nobservers", {
  expect_error(
    randomise(ncomp = ni,
              nobservers = NULL,
              nitems = nv,
              itemnames = inames)
  )
})

test_that("missing nitems", {
  expect_error(
    randomise(ncomp = ni,
              nobservers = no,
              nitems = NULL,
              itemnames = inames)
  )
})

test_that("missing itemnames", {
  expect_error(
    randomise(ncomp = ni,
              nobservers = no,
              nitems = nv,
              itemnames = NULL)
  )
})

test_that("less than 3 items", {
  expect_error(
    randomise(ncomp = ni,
              nobservers = no,
              nitems = 2,
              itemnames = c("A", "B"))
  )
})


test_that("alias works", {
  r <- randomize(ncomp = ni,
                 nobservers = no,
                 nitems = nv,
                 itemnames = inames)
  
  r <- as.vector(apply(r, 1, is.na))
  
  r <- sum(r) == 0
  
  expect_true(r)
  
})

test_that("alias works", {
  
  ni <- 3
  no <- 20
  nv <- 7
  inames <- paste("Var", 1:nv, sep="")
  
  r <- randomize(ncomp = ni,
                 nobservers = no,
                 nitems = nv,
                 itemnames = inames)
  

  r <- is.data.frame(r)
  
  expect_true(r)
  
})

