test_that("get_globalcc calculates the expected global clustering coefficient", {

  # Four-node network containing two of four possible triangles:
  # 1-2-3 and 2-3-4.
  # Therefore GCC = 2 / choose(4, 3) = 0.5.
  mi <- matrix(c(
    0,   0.8, 0.6, 0,
    0.8, 0,   0.4, 0.7,
    0.6, 0.4, 0,   0.5,
    0,   0.7, 0.5, 0
  ), nrow = 4, byrow = TRUE)

  expect_equal(get_globalcc(mi), 0.5)
})

test_that("get_globalcc handles networks with no and all possible triangles", {

  # No edges -> no triangles
  empty_network <- matrix(0, nrow = 4, ncol = 4)

  expect_equal(get_globalcc(empty_network), 0)

  # Complete four-node network -> every possible triple is a triangle
  complete_network <- matrix(1, nrow = 4, ncol = 4)
  diag(complete_network) <- 0

  expect_equal(get_globalcc(complete_network), 1)
})
