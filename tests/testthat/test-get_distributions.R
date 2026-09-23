test_that("get_distributions returns px and ex for each variable", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6),
    c(2, 3, 4, 5, 6, 7)
  )

  result <- get_distributions(data)

  expect_type(result, "list")
  expect_named(result, c("px", "ex"))
  expect_length(result$px, 2)
  expect_length(result$ex, 2)

  expect_equal(sum(result$px[[1]]), 1)
  expect_equal(sum(result$px[[2]]), 1)
})


test_that("get_distributions uses MATLAB-compatible IQR for Freedman-Diaconis", {
  data <- matrix(
    c(23, 23, 18, 17, 14, 9, 14, 18, 19, 11,
      18, 18, 19, 17, 14, 18, 18, 19, 22, 24),
    nrow = 1
  )

  result <- get_distributions(data, binning = "freedman")

  iqr_matlab <- 3.5
  expected_width <- 2 * iqr_matlab / ncol(data)^(1 / 3)

  expected_edges <- seq(
    min(data),
    max(data),
    by = expected_width
  )

  expect_equal(result$ex[[1]], expected_edges)
})


test_that("get_distributions handles Sturges binning", {
  data <- matrix(1:20, nrow = 1)

  result <- get_distributions(data, binning = "sturges")

  nbins <- ceiling(1 + log2(ncol(data)))

  expect_length(result$ex[[1]], nbins + 1)
  expect_equal(result$ex[[1]][1], min(data))
  expect_equal(
    result$ex[[1]][length(result$ex[[1]])],
    max(data)
  )
  expect_equal(sum(result$px[[1]]), 1)
})


test_that("get_distributions skips all-zero variables", {
  data <- rbind(
    rep(0, 10),
    1:10
  )

  result <- get_distributions(data)

  expect_null(result$px[[1]])
  expect_null(result$ex[[1]])

  expect_false(is.null(result$px[[2]]))
  expect_false(is.null(result$ex[[2]]))
})


test_that("get_distributions handles constant nonzero variables", {
  data <- matrix(rep(5, 10), nrow = 1)

  result <- get_distributions(data, binning = "freedman")

  expect_false(is.null(result$px[[1]]))
  expect_equal(sum(result$px[[1]]), 1)
})


test_that("get_distributions returns original-scale edges after log transformation", {
  data <- matrix(1:20, nrow = 1)

  result <- get_distributions(
    data,
    binning = "sturges",
    log = TRUE
  )

  expect_equal(result$ex[[1]][1], min(data))
  expect_equal(
    result$ex[[1]][length(result$ex[[1]])],
    max(data)
  )
  expect_equal(sum(result$px[[1]]), 1)
})
