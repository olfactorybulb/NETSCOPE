test_that("get_pathlength calculates length of a single path", {
  mi <- matrix(
    c(
      0,   0.5, 0,
      0.5, 0,   0.25,
      0,   0.25, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- get_pathlength(mi, c(1, 2, 3))

  expect_equal(result, 4)
})


test_that("get_pathlength calculates lengths for multiple paths", {
  mi <- matrix(
    c(
      0,   0.5, 0.2,
      0.5, 0,   0.25,
      0.2, 0.25, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  paths <- list(
    c(1, 2),
    c(1, 2, 3),
    c(1, 3)
  )

  result <- get_pathlength(mi, paths)

  expect_equal(result, c(1, 4, 4))
})


test_that("get_pathlength returns zero for empty and single-node paths", {
  mi <- matrix(
    c(
      0,   0.5,
      0.5, 0
    ),
    nrow = 2,
    byrow = TRUE
  )

  paths <- list(
    NULL,
    1
  )

  result <- get_pathlength(mi, paths)

  expect_equal(result, c(0, 0))
})

test_that("get_pathlength and shortestpath use consistent distances", {
  mi <- matrix(c(
    0, 1 + 1e-15, 0.2,
    1 + 1e-15, 0, 0.5,
    0.2, 0.5, 0
  ), nrow = 3, byrow = TRUE)

  result <- shortestpath(mi, source = 1, target = 3)

  expect_equal(
    get_pathlength(mi, result$paths[[1]]),
    result$d
  )

  expect_gte(get_pathlength(mi, c(1, 2)), 0)
  expect_equal(get_pathlength(mi, c(1, 2)), 0)
})

test_that("get_pathlength clamps MI values to [0, 1]", {
  mi <- matrix(c(
    0, 1.2, -0.1,
    1.2, 0, 0.5,
    -0.1, 0.5, 0
  ), nrow = 3, byrow = TRUE)

  # MI > 1 should be clamped to 1
  expect_equal(get_pathlength(mi, c(1, 2)), 0)

  # MI < 0 should be clamped to 0
  expect_equal(get_pathlength(mi, c(1, 3)), Inf)

  # Values within [0, 1] should remain unchanged
  expect_equal(get_pathlength(mi, c(2, 3)), 1)
})
