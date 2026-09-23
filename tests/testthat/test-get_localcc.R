test_that("get_localcc returns one for a fully connected triangle", {
  mi <- matrix(
    c(
      0, 0.8, 0.6,
      0.8, 0, 0.7,
      0.6, 0.7, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- get_localcc(mi)

  expect_equal(result, c(1, 1, 1))
})


test_that("get_localcc returns zero when neighbors are not connected", {
  mi <- matrix(
    c(
      0, 0.8, 0.6,
      0.8, 0, 0,
      0.6, 0, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- get_localcc(mi)

  expect_equal(result, c(0, 0, 0))
})


test_that("get_localcc depends on connectivity rather than MI magnitude", {
  mi1 <- matrix(
    c(
      0, 0.2, 0.3,
      0.2, 0, 0.4,
      0.3, 0.4, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  mi2 <- matrix(
    c(
      0, 0.9, 0.7,
      0.9, 0, 0.8,
      0.7, 0.8, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_equal(
    get_localcc(mi1),
    get_localcc(mi2)
  )
})
