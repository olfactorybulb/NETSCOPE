test_that("get_entropy computes entropy correctly", {
  px <- list(
    c(0.5, 0.5),
    c(0.25, 0.25, 0.25, 0.25)
  )

  result <- get_entropy(px)

  expected <- c(
    log(2),
    log(4)
  )

  expect_equal(result, expected)
})


test_that("get_entropy returns zero for a deterministic distribution", {
  px <- list(c(1, 0, 0))

  result <- get_entropy(px)

  expect_equal(result, 0)
})


test_that("get_entropy handles NULL distributions", {
  px <- list(
    c(0.5, 0.5),
    NULL,
    c(0.25, 0.75)
  )

  result <- get_entropy(px)

  expect_equal(length(result), 3)
  expect_equal(result[1], log(2))
  expect_equal(result[2], 0)
  expect_equal(result[3], -(0.25 * log(0.25) + 0.75 * log(0.75)))
})
