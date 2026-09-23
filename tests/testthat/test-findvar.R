test_that("findvar finds names by substring", {
  vars <- c("alpha", "beta", "alphabet", "gamma")

  result <- findvar(vars, "alpha")

  expect_equal(result, c(1L, 3L))
})


test_that("findvar is case-insensitive", {
  vars <- c("Alpha", "BETA", "Gamma")

  result <- findvar(vars, "ALPHA")

  expect_equal(result, 1L)
})


test_that("findvar accepts multiple search terms", {
  vars <- c("apple", "banana", "cherry", "blueberry")

  separate <- findvar(vars, "app", "berry")
  bundled <- findvar(vars, c("app", "berry"))

  expect_equal(separate, c(1L, 4L))
  expect_equal(bundled, c(1L, 4L))
})


test_that("findvar returns integer(0) when nothing matches", {
  vars <- c("alpha", "beta", "gamma")

  result <- findvar(vars, "xyz")

  expect_identical(result, integer(0))
})
