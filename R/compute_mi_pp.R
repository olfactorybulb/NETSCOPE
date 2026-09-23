#' Compute MI matrix in batches, using parallel processing
#'
#' Internal function called from \code{compute_MI_batch()} when
#' \code{pp = TRUE}. Dispatches one asynchronous job per batch block via the
#' \pkg{future} ecosystem, storing job/output state in
#' \code{.netscope_pp_state} so that \code{fetch_results()} can later
#' collect the results (possibly from a separate call, mirroring MATLAB's
#' background \code{parfeval} pool).
#'
#' @param data Numeric matrix (rows = variables, columns = samples).
#' @param file Output file path (without extension) backing the result
#'   matrix (see \code{compute_MI_batch()}).
#' @param px List of marginal distributions.
#' @param ex List of bin edges.
#' @param h Numeric vector of per-variable entropies.
#' @param batch Batch size.
#' @param fetch Whether to block and fetch results immediately, or return
#'   right away and leave jobs running in the background.
#' @param cont Whether to continue from a previous, unfinished run.
#'
#' @section Unresolved dependencies:
#' Calls \code{fetch_results()}, not yet translated from source.
#'
#' @keywords internal
compute_MI_PP <- function(data, file, px, ex, h, batch, fetch, cont) {

  ## Set up batch structure -----------------------------------------------------
  message(sprintf("Start time: %s", format(Sys.time())))
  nvars <- nrow(data)

  # Ensure a parallel backend is active without overriding a plan the user
  # already set deliberately (mirrors MATLAB's gcp(), which creates a pool
  # only if none exists)
  if (inherits(future::plan(), "sequential")) {
    future::plan(future::multisession)
  }

  # Temporary file for provisional results -- workers load data/px/ex from
  # here independently, since (unlike MATLAB's shared-memory local pool)
  # future workers may not share memory with the main session
  tempfile_path <- tempfile()
  saveRDS(list(data = data, px = px, ex = ex, h = h), tempfile_path)

  # Open output file, allocate matrix if it doesn't exist
  backing_dir <- dirname(normalizePath(file, mustWork = FALSE))
  backing_file <- paste0(basename(file), ".bin")
  descriptor_file <- paste0(basename(file), ".desc")

  if (!cont) {
    message("Allocating memory for matrix")
    outfile <- bigmemory::filebacked.big.matrix(
      nrow = nvars, ncol = nvars, init = 0,
      backingfile = backing_file, backingpath = backing_dir,
      descriptorfile = descriptor_file
    )
  } else {
    outfile <- bigmemory::attach.big.matrix(
      file.path(backing_dir, descriptor_file)
    )
  }

  ## Create parallel batch jobs -------------------------------------------------
  jobs <- list()
  lp <- seq(1, nvars, by = batch)  # loop index

  for (i in seq_along(lp)) {
    ix <- lp[i]:min(nvars, lp[i] + batch - 1)

    for (j in seq_len(i)) {
      jx <- lp[j]:min(nvars, lp[j] + batch - 1)

      if (cont && sum(outfile[ix, jx]) > 0) next

      job <- future::future(
        process_batch(tempfile_path, ix, jx),
        seed = TRUE,
        packages = "NETSCOPE"
      )
      jobs[[length(jobs) + 1]] <- job
    }
  }

  # Store state for fetch_results() to pick up, mirroring MATLAB's globals
  assign("jobs", jobs, envir = .netscope_pp_state)
  assign("outfile", outfile, envir = .netscope_pp_state)
  assign("tempfile_path", tempfile_path, envir = .netscope_pp_state)

  # Start fetching data right away, freeing the large local objects first
  # (approximates MATLAB's `clear;` before the blocking fetch phase)
  if (fetch) {
    rm(data, px, ex)
    gc()
    fetch_results()
  }

  invisible(NULL)
}
