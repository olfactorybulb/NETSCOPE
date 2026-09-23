test_that("compute_MI returns a symmetric MI matrix", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
    c(10, 3, 7, 1, 8, 4, 9, 2, 6, 5)
  )

  result <- compute_MI(data)

  expect_equal(dim(result), c(3, 3))
  expect_equal(result, t(result))
})


test_that("compute_MI returns zero diagonal", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  )

  result <- compute_MI(data)

  expect_equal(diag(result), c(0, 0))
})


test_that("compute_MI can use precomputed distributions and entropy", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(2, 3, 5, 4, 7, 6, 9, 8, 10, 11)
  )

  distributions <- get_distributions(data)
  h <- get_entropy(distributions$px)

  result_auto <- compute_MI(data)

  result_precomputed <- compute_MI(
    data,
    px = distributions$px,
    ex = distributions$ex,
    h = h
  )

  expect_equal(result_auto, result_precomputed)
})


test_that("compute_MI handles all-zero variables", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    rep(0, 10)
  )

  result <- compute_MI(data)

  expect_equal(dim(result), c(2, 2))
  expect_equal(result[2, ], c(0, 0))
  expect_equal(result[, 2], c(0, 0))
})
