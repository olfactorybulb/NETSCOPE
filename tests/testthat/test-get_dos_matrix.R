test_that("get_dos_matrix returns correct hop counts", {
  mi <- matrix(
    c(
      0,   0.8, 0,   0,
      0.8, 0,   0.6, 0.7,
      0,   0.6, 0,   0,
      0,   0.7, 0,   0
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- get_dos_matrix(mi)

  expected <- matrix(
    c(
      0, 1, 2, 2,
      1, 0, 1, 1,
      2, 1, 0, 2,
      2, 1, 2, 0
    ),
    nrow = 4,
    byrow = TRUE
  )

  expect_equal(result, expected)
})


test_that("get_dos_matrix depends on connectivity rather than MI magnitude", {
  mi1 <- matrix(
    c(
      0,   0.2, 0,
      0.2, 0,   0.3,
      0,   0.3, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  mi2 <- matrix(
    c(
      0,   0.9, 0,
      0.9, 0,   0.7,
      0,   0.7, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_equal(
    get_dos_matrix(mi1),
    get_dos_matrix(mi2)
  )
})


test_that("get_dos_matrix returns zero diagonal", {
  mi <- matrix(
    c(
      0,   0.5, 0,
      0.5, 0,   0.5,
      0,   0.5, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- get_dos_matrix(mi)

  expect_equal(diag(result), c(0, 0, 0))
})
