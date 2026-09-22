test_that("sparsify_network removes the weakest edge according to DPI", {

  mi <- matrix(c(
    0,   0.8, 0.6, 0,
    0.8, 0,   0.4, 0.7,
    0.6, 0.4, 0,   0.5,
    0,   0.7, 0.5, 0
  ), nrow = 4, byrow = TRUE)

  result <- sparsify_network(mi)

  expected_sps <- matrix(c(
    0,   0.8, 0.6, 0,
    0.8, 0,   0,   0.7,
    0.6, 0,   0,   0.5,
    0,   0.7, 0.5, 0
  ), nrow = 4, byrow = TRUE)

  expect_equal(result$sps, expected_sps)
  expect_equal(result$gcc, 0.5)
})
