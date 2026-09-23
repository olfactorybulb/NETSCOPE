test_that("normalize_MI correctly normalizes MI and calculates distance", {
  mi_unnorm <- matrix(
    c(1, 0.5,
      0.5, 2),
    nrow = 2,
    byrow = TRUE
  )

  hi <- c(1, 2)

  result <- normalize_MI(mi_unnorm, hi)

  expected_mi <- matrix(
    c(1,   0.2,
      0.2, 1),
    nrow = 2,
    byrow = TRUE
  )

  expected_dist <- matrix(
    c(0, 4,
      4, 0),
    nrow = 2,
    byrow = TRUE
  )

  expect_equal(result$mi, expected_mi)
  expect_equal(result$dist, expected_dist)
})


test_that("normalize_MI uses hi as hj by default", {
  mi_unnorm <- matrix(
    c(1, 0.5,
      0.5, 2),
    nrow = 2,
    byrow = TRUE
  )

  hi <- c(1, 2)

  result_default <- normalize_MI(mi_unnorm, hi)
  result_explicit <- normalize_MI(mi_unnorm, hi, hj = hi)

  expect_equal(result_default, result_explicit)
})


test_that("normalize_MI accepts different hi and hj vectors", {
  mi_unnorm <- matrix(
    c(0.5, 0.25,
      0.2,  0.4),
    nrow = 2,
    byrow = TRUE
  )

  hi <- c(1, 2)
  hj <- c(1.5, 2.5)

  result <- normalize_MI(mi_unnorm, hi, hj)

  joint_h <- outer(hi, hj, "+") - mi_unnorm
  expected_mi <- mi_unnorm / joint_h
  expected_dist <- (1 - expected_mi) / expected_mi

  expect_equal(result$mi, expected_mi)
  expect_equal(result$dist, expected_dist)
})


test_that("normalize_MI replaces NaN normalized MI values with zero", {
  mi_unnorm <- matrix(0, nrow = 1, ncol = 1)
  hi <- 0

  result <- normalize_MI(mi_unnorm, hi)

  expect_equal(result$mi, matrix(0, 1, 1))
  expect_true(is.infinite(result$dist[1, 1]))
})
