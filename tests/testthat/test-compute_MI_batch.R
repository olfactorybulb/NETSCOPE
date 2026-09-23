test_that("compute_MI_batch matches compute_MI", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
    c(10, 3, 7, 1, 8, 4, 9, 2, 6, 5)
  )

  expected <- compute_MI(data)

  file <- tempfile()

  outfile <- compute_MI_batch(
    data,
    file = file,
    batch = 2,
    pp = FALSE
  )

  result <- outfile[, ]

  expect_equal(result, expected)

  unlink(paste0(file, ".bin"))
  unlink(paste0(file, ".desc"))
})


test_that("compute_MI_batch gives same result across batch sizes", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
    c(10, 3, 7, 1, 8, 4, 9, 2, 6, 5)
  )

  file1 <- tempfile()
  file2 <- tempfile()

  result1 <- compute_MI_batch(
    data,
    file = file1,
    batch = 1,
    pp = FALSE
  )

  result2 <- compute_MI_batch(
    data,
    file = file2,
    batch = 3,
    pp = FALSE
  )

  expect_equal(result1[, ], result2[, ])

  unlink(c(
    paste0(file1, ".bin"),
    paste0(file1, ".desc"),
    paste0(file2, ".bin"),
    paste0(file2, ".desc")
  ))
})


test_that("compute_MI_batch can use precomputed distributions and entropy", {
  data <- rbind(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    c(2, 3, 5, 4, 7, 6, 9, 8, 10, 11)
  )

  distributions <- get_distributions(data)
  h <- get_entropy(distributions$px)

  file1 <- tempfile()
  file2 <- tempfile()

  auto <- compute_MI_batch(
    data,
    file = file1,
    batch = 1,
    pp = FALSE
  )

  precomputed <- compute_MI_batch(
    data,
    file = file2,
    px = distributions$px,
    ex = distributions$ex,
    h = h,
    batch = 1,
    pp = FALSE
  )

  expect_equal(auto[, ], precomputed[, ])

  unlink(c(
    paste0(file1, ".bin"),
    paste0(file1, ".desc"),
    paste0(file2, ".bin"),
    paste0(file2, ".desc")
  ))
})
