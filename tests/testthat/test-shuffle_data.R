test_that("shuffle_data preserves variable marginals by default", {
  x <- matrix(c(
    20, 25, 30, 35,
    80, 85, 90, 95,
    1,  2,  3,  4
  ), nrow = 3, byrow = TRUE)

  set.seed(123)
  shuffled <- shuffle_data(x)

  # Dimensions are preserved
  expect_equal(dim(shuffled), dim(x))

  # Each variable retains its original values
  for (i in seq_len(nrow(x))) {
    expect_equal(sort(shuffled[i, ]), sort(x[i, ]))
  }

  # Default is equivalent to margin = "variables"
  set.seed(123)
  explicit <- shuffle_data(x, margin = "variables")

  expect_equal(shuffled, explicit)
})

test_that("shuffle_data does not exchange values across variables by default", {
  x <- matrix(c(
    20, 25, 30, 35,
    80, 85, 90, 95,
    1,  2,  3,  4
  ), nrow = 3, byrow = TRUE)

  set.seed(123)
  shuffled <- shuffle_data(x)

  expect_true(all(shuffled[1, ] %in% x[1, ]))
  expect_true(all(shuffled[2, ] %in% x[2, ]))
  expect_true(all(shuffled[3, ] %in% x[3, ]))
})


test_that("shuffle_data preserves sample values with margin = 'samples'", {
  x <- matrix(c(
    20, 25, 30, 35,
    80, 85, 90, 95,
    1,  2,  3,  4
  ), nrow = 3, byrow = TRUE)

  set.seed(123)
  shuffled <- shuffle_data(x, margin = "samples")

  # Dimensions are preserved
  expect_equal(dim(shuffled), dim(x))

  # Each sample retains its original values
  for (i in seq_len(ncol(x))) {
    expect_equal(sort(shuffled[, i]), sort(x[, i]))
  }
})


test_that("shuffle_data is reproducible with set.seed", {
  x <- matrix(1:20, nrow = 4)

  set.seed(123)
  result1 <- shuffle_data(x)

  set.seed(123)
  result2 <- shuffle_data(x)

  expect_equal(result1, result2)

  # Check reproducibility for the original MATLAB behavior
  set.seed(123)
  result3 <- shuffle_data(x, margin = "samples")

  set.seed(123)
  result4 <- shuffle_data(x, margin = "samples")

  expect_equal(result3, result4)
})


test_that("shuffle_data rejects invalid margin arguments", {
  x <- matrix(1:12, nrow = 3)

  expect_error(
    shuffle_data(x, margin = "invalid")
  )
})
