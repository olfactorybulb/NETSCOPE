#' Compute mutual information (MI) matrix from continuous data, in batches
#'
#' Computes a normalized MI matrix where each entry (i, j) is the MI
#' between variables i and j. Intended for datasets too large to comfortably
#' hold as a single in-memory result matrix: this function does not require
#' the full MI matrix to live in RAM, computing it in batches and storing
#' the result in a disk-backed matrix (via \pkg{bigmemory}). Optionally uses
#' parallel processing (via the \pkg{future} ecosystem).
#'
#' @param data Numeric matrix (rows = variables, columns = samples).
#' @param file Output file path (without extension) to back the result
#'   matrix. Two files will be created: \code{<file>.bin} and
#'   \code{<file>.desc}.
#' @param px Optional list of marginal distributions, as produced by
#'   \code{get_distributions()}. Computed if not supplied.
#' @param ex Optional list of bin edges. Required if \code{px} is supplied.
#' @param h Optional numeric vector of per-variable entropies, as produced
#'   by \code{get_entropy()}. Computed if not supplied.
#' @param batch Batch size. Default \code{1000}.
#' @param pp Whether to use parallel processing via \code{compute_MI_PP()}.
#'   Default \code{FALSE}.
#' @param fetch Whether to wait for parallel processing to finish before
#'   returning. Only relevant when \code{pp = TRUE}. Default \code{TRUE}.
#' @param cont Whether to continue from a previous, unfinished run (attaches
#'   to the existing backing file instead of reallocating). Default
#'   \code{FALSE}.
#'
#' @return Invisibly, a \code{big.matrix} object containing the disk-backed
#'   MI matrix. The matrix data are stored at \code{<file>.bin}.
#'
#' @seealso \code{compute_MI}, \code{normalize_MI}
#'
#' @examples
#' # Create a small example dataset
#' set.seed(123)
#' data <- matrix(rnorm(300), nrow = 3)
#'
#' # Store the disk-backed MI matrix in a temporary directory
#' file <- file.path(tempdir(), "netscope_mi")
#' mi <- compute_MI_batch(data, file, batch = 2)
#'
#' # Access the resulting matrix
#' mi[, ]
#'
#' @export
compute_MI_batch <- function(data, file, px = NULL, ex = NULL, h = NULL,
                             batch = 1000, pp = FALSE, fetch = TRUE,
                             cont = FALSE) {

  ## Parse arguments, compute px, h if necessary ------------------------------
  if (is.null(px)) {
    distributions <- get_distributions(data)
    px <- distributions$px
    ex <- distributions$ex
  }
  if (is.null(h)) {
    h <- get_entropy(px)
  }

  # Use separate function for parallel processing
  if (pp) {
    return(compute_MI_PP(data, file, px, ex, h, batch, fetch, cont))
  }

  ## Set up batch structure -----------------------------------------------------
  message(sprintf("Start time: %s", format(Sys.time())))
  nvars <- nrow(data)

  backing_dir <- dirname(normalizePath(file, mustWork = FALSE))
  backing_file <- paste0(basename(file), ".bin")
  descriptor_file <- paste0(basename(file), ".desc")

  # Open output file, allocate matrix if it doesn't exist
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

  batched_data <- list(data = data, px = px, ex = ex)

  ## Create batch jobs (sequential) --------------------------------------------
  lp <- seq(1, nvars, by = batch)  # loop index
  nbatches <- 0.5 * length(lp) * (length(lp) + 1)
  k <- 0

  for (i in seq_along(lp)) {
    ix <- lp[i]:min(nvars, lp[i] + batch - 1)

    for (j in seq_len(i)) {
      jx <- lp[j]:min(nvars, lp[j] + batch - 1)
      k <- k + 1

      if (cont && sum(outfile[ix, jx]) > 0) next

      # process_batch() returns a list (mi, ix, jx); ix/jx pass through
      # unchanged and only matter for async parallel bookkeeping, so only
      # $mi is needed here in the sequential path
      mi_block <- process_batch(batched_data, ix, jx)$mi
      mi_block <- normalize_MI(mi_block, h[ix], h[jx])$mi

      outfile[ix, jx] <- mi_block
      if (i != j) {  # not along diagonal
        outfile[jx, ix] <- t(mi_block)
      }

      message(sprintf("%s: Saved batch %d out of %d",
                      format(Sys.time()), k, nbatches))
    }
  }

  invisible(outfile)
}
