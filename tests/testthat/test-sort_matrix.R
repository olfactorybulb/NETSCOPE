test_that("sort_matrix returns a valid permutation of connected nodes", {
  mi <- matrix(
    c(
      0,   0.8, 0.6, 0.2,
      0.8, 0,   0.4, 0.7,
      0.6, 0.4, 0,   0.5,
      0.2, 0.7, 0.5, 0
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- sort_matrix(mi)

  expect_equal(sort(result$order), 1:4)
  expect_equal(dim(result$max_comp), c(4, 3))
})


test_that("sort_matrix respects requested number of components", {
  mi <- matrix(
    c(
      0,   0.8, 0.6, 0.2,
      0.8, 0,   0.4, 0.7,
      0.6, 0.4, 0,   0.5,
      0.2, 0.7, 0.5, 0
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- sort_matrix(mi, ncomps = 2)

  expect_equal(dim(result$max_comp), c(4, 2))
  expect_equal(sort(result$order), 1:4)
})


test_that("sort_matrix places disconnected nodes at the end", {
  mi <- matrix(
    c(
      0,   0.8, 0.6, 0,
      0.8, 0,   0.4, 0,
      0.6, 0.4, 0,   0,
      0,   0,   0,   0
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- sort_matrix(mi)

  expect_true(all(is.na(result$max_comp[4, ])))
  expect_equal(tail(result$order, 1), 4)
})
