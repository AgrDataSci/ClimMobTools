design_counts = function(d) table(unlist(d))

test_that("every package holds distinct items and no missing values", {
  set.seed(1)
  d = randomise(npackages = 30, itemnames = paste0("Var", 1:6), ncomp = 3)
  expect_equal(dim(d), c(30L, 3L))
  expect_false(anyNA(d))
  expect_true(all(apply(d, 1, function(z) !anyDuplicated(z))))
})

test_that("items appear equally often when proportions are equal", {
  set.seed(2)
  d = randomise(npackages = 10, itemnames = paste0("Var", 1:4), ncomp = 3)
  counts = design_counts(d)
  # 30 slots over 4 items: 7 or 8 each
  expect_true(all(counts %in% c(7, 8)))
})

test_that("each item is spread over the positions A, B and C", {
  set.seed(3)
  d = randomise(npackages = 60, itemnames = paste0("Var", 1:6), ncomp = 3)
  pos = sapply(d, function(col) table(factor(col, levels = paste0("Var", 1:6))))
  # each item appears 30 times; positions should be close to 10 each
  expect_true(all(apply(pos, 1, function(z) max(z) - min(z)) <= 2))
})

test_that("the comparison network connects all items", {
  set.seed(4)
  items = paste0("Var", 1:10)
  d = randomise(npackages = 40, itemnames = items, ncomp = 3)
  # union-find over the items compared in each package
  parent = stats::setNames(items, items)
  find = function(x) { while (parent[[x]] != x) x = parent[[x]]; x }
  for (i in seq_len(nrow(d))) {
    r = vapply(unlist(d[i, ]), find, character(1))
    parent[r] = r[1]
  }
  roots = unique(vapply(items, find, character(1)))
  expect_length(roots, 1)
})

test_that("more than three items per package works", {
  set.seed(5)
  d = randomise(npackages = 20, itemnames = paste0("Var", 1:10), ncomp = 5)
  expect_equal(ncol(d), 5L)
  expect_false(anyNA(d))
})

test_that("availability is respected and checked", {
  set.seed(6)
  avail = c(5, 8, 50, 50, 50)
  d = randomise(npackages = 20, itemnames = c("a", "b", "c", "d", "e"),
                ncomp = 3, availability = avail)
  counts = design_counts(d)[c("a", "b", "c", "d", "e")]
  expect_true(all(counts <= avail))
  expect_error(randomise(npackages = 20, itemnames = c("a", "b", "c"),
                         ncomp = 3, availability = c(5, 5, 5)))
})

test_that("the same seed gives the same design", {
  set.seed(7)
  d1 = randomise(npackages = 15, itemnames = paste0("Var", 1:5), ncomp = 3)
  set.seed(7)
  d2 = randomise(npackages = 15, itemnames = paste0("Var", 1:5), ncomp = 3)
  expect_identical(d1, d2)
})
