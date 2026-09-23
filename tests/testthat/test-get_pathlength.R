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
