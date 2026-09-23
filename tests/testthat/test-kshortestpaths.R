test_that("kshortestpaths finds alternative paths", {
  mi <- matrix(
    c(
      0,   0.5, 0.5, 0,
      0.5, 0,   0,   0.5,
      0.5, 0,   0,   0.5,
      0,   0.5, 0.5, 0
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- kshortestpaths(
    mi,
    K = 2,
    source = 1,
    target = 4
  )

  expect_length(result$A, 2)
  expect_equal(result$d, c(2, 2))

  expect_true(
    identical(result$A[[1]], c(1L, 2L, 4L)) ||
      identical(result$A[[1]], c(1L, 3L, 4L))
  )

  expect_true(
    identical(result$A[[2]], c(1L, 2L, 4L)) ||
      identical(result$A[[2]], c(1L, 3L, 4L))
  )

  expect_false(identical(result$A[[1]], result$A[[2]]))
})

test_that("kshortestpaths orders paths by path length", {
  mi <- matrix(
    c(
      0,   0.5, 0.25, 0,
      0.5, 0,   0,    0.5,
      0.25, 0,  0,    0.25,
      0,   0.5, 0.25, 0
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- kshortestpaths(
    mi,
    K = 2,
    source = 1,
    target = 4
  )

  expect_equal(result$A[[1]], c(1L, 2L, 4L))
  expect_equal(result$A[[2]], c(1L, 3L, 4L))

  expect_equal(result$d, c(2, 6))
})
