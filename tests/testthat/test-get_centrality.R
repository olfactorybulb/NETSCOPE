test_that("get_centrality identifies the middle node of a line network", {
  mi <- matrix(0, 3, 3)

  paths <- list(
    list(
      c(1),
      c(1, 2),
      c(1, 2, 3)
    ),
    list(
      c(2, 1),
      c(2),
      c(2, 3)
    ),
    list(
      c(3, 2, 1),
      c(3, 2),
      c(3)
    )
  )

  result <- get_centrality(mi, paths = paths)

  expect_equal(result, c(0, 1 / 3, 0))
})


test_that("get_centrality returns zero when all nodes are directly connected", {
  mi <- matrix(0, 3, 3)

  paths <- list(
    list(c(1), c(1, 2), c(1, 3)),
    list(c(2, 1), c(2), c(2, 3)),
    list(c(3, 1), c(3, 2), c(3))
  )

  result <- get_centrality(mi, paths = paths)

  expect_equal(result, c(0, 0, 0))
})


test_that("get_centrality can compute shortest paths internally", {
  mi <- matrix(
    c(
      0,   0.5, 0,
      0.5, 0,   0.5,
      0,   0.5, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- get_centrality(mi)

  expect_equal(result, c(0, 1 / 3, 0))
})
