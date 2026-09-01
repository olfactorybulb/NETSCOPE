#' Fetch results from parallel MI batch jobs
#'
#' Internal function called from \code{compute_MI_PP()}. Drains the job
#' queue stored in \code{.netscope_pp_state}, normalizing and writing each
#' finished batch's MI block into the disk-backed output matrix. Can be
#' interrupted and called again later, as long as the jobs recorded in
#' \code{.netscope_pp_state} are still valid \pkg{future} objects (i.e. the
#' parallel workers are still alive). If not, call \code{compute_MI_batch()}
#' with \code{pp = TRUE} and \code{cont = TRUE} to resume computation from
#' the partially-filled output matrix on disk.
#'
#' @return \code{NULL}, invisibly. Called for its side effect of populating
#'   the output matrix stored in \code{.netscope_pp_state$outfile}.
#'
#' @keywords internal
fetch_results <- function() {
  if (!exists("jobs", envir = .netscope_pp_state, inherits = FALSE)) {
    stop("No futures running, function aborted")
  }
  jobs <- get("jobs", envir = .netscope_pp_state)
  outfile <- get("outfile", envir = .netscope_pp_state)
  tempfile_path <- get("tempfile_path", envir = .netscope_pp_state)

  if (length(jobs) == 0) {
    stop("No futures running, function aborted")
  }

  # Load shared entropy vector saved to disk by compute_MI_PP()
  state <- readRDS(tempfile_path)
  h <- state$h

  message("Fetching results from parallel jobs")
  nbatches <- length(jobs)
  k <- 1L  # Number of finished batches

  while (length(jobs) > 0) {
    # future has no direct fetchNext() equivalent; poll for the first
    # resolved job (order of completion, not submission order)
    resolved <- vapply(jobs, future::resolved, logical(1))
    j <- which(resolved)[1]
    if (is.na(j)) {
      Sys.sleep(0.1)
      next
    }

    result <- future::value(jobs[[j]])
    jobs[[j]] <- NULL
    # Persist shrinking job list so an interrupted run can resume later
    assign("jobs", jobs, envir = .netscope_pp_state)

    mi <- result$mi
    ix <- result$ix
    jx <- result$jx

    mi <- normalize_MI(mi, h[ix], h[jx])$mi
    outfile[ix, jx] <- mi
    if (!identical(ix, jx)) {  # Not along diagonal
      outfile[jx, ix] <- t(mi)
    }

    message(sprintf("%s: Saved batch %d out of %d", format(Sys.time()), k, nbatches))
    k <- k + 1L
  }

  # Clean up
  unlink(tempfile_path)
  rm(list = c("jobs", "outfile", "tempfile_path"), envir = .netscope_pp_state)
  # future workers are torn down via future::plan(future::sequential) by the
  # caller if desired; unlike MATLAB's ppool, there's no single pool object
  # to delete() here

  invisible(NULL)
}
