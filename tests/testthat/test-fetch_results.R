test_that("fetch_results errors when no futures are running", {
  # Ensure no leftover state from another test
  for (x in c("jobs", "outfile", "tempfile_path")) {
    if (exists(x, envir = .netscope_pp_state, inherits = FALSE)) {
      rm(list = x, envir = .netscope_pp_state)
    }
  }

  expect_error(
    fetch_results(),
    "No futures running, function aborted"
  )
})


test_that("fetch_results writes completed results and cleans up", {
  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(list(h = c(1, 1)), temp_rds)

  backing_dir <- tempdir()
  backing_file <- paste0("fetch-", basename(tempfile()), ".bin")
  descriptor_file <- paste0("fetch-", basename(tempfile()), ".desc")

  outfile <- bigmemory::filebacked.big.matrix(
    nrow = 2,
    ncol = 2,
    init = 0,
    backingfile = backing_file,
    backingpath = backing_dir,
    descriptorfile = descriptor_file
  )

  # Clean up even if the test fails
  on.exit({
    for (x in c("jobs", "outfile", "tempfile_path")) {
      if (exists(x, envir = .netscope_pp_state, inherits = FALSE)) {
        rm(list = x, envir = .netscope_pp_state)
      }
    }

    unlink(temp_rds)
    unlink(file.path(backing_dir, backing_file))
    unlink(file.path(backing_dir, descriptor_file))
  }, add = TRUE)

  job <- future::future({
    list(
      mi = matrix(0.5, nrow = 1, ncol = 1),
      ix = 1L,
      jx = 2L
    )
  })

  assign("jobs", list(job), envir = .netscope_pp_state)
  assign("outfile", outfile, envir = .netscope_pp_state)
  assign("tempfile_path", temp_rds, envir = .netscope_pp_state)

  fetch_results()

  expect_equal(outfile[1, 2], 1 / 3)
  expect_equal(outfile[2, 1], 1 / 3)
  expect_false(file.exists(temp_rds))
})
