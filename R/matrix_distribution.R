#' Get distribution of data in a matrix
#'
#' For a matrix too large to load completely into memory, a disk-backed
#' \pkg{bigmemory} matrix file can be specified instead, so the data is
#' read and processed in row batches. This function assumes the data is
#' confined to the interval \code{[0, 1]}.
#'
#' @param file Optional path, without extension, to a \pkg{bigmemory}
#'   backing file created by NETSCOPE. Required if \code{mat} is not
#'   specified.
#' @param mat Optional in-memory matrix. Required if \code{file} is not
#'   specified.
#' @param batch Batch size for reading rows at a time. Default 4000.
#' @param bins Number of bins used for counting. Default 1000.
#'
#' @return A list with:
#'   \item{px}{Data distribution -- vector of bin counts.}
#'   \item{ex}{Bin edges.}
#'
#' @examples
#' # Create a matrix with values between 0 and 1
#' set.seed(123)
#' mat <- matrix(runif(100), nrow = 10)
#'
#' # Compute the distribution
#' result <- matrix_distribution(mat = mat)
#' result$px
#'
#' @family other
#' @export
matrix_distribution <- function(file = NULL, mat = NULL, batch = 4000, bins = 1000) {
  if (!is.null(file)) {
    backing_dir <- dirname(normalizePath(file, mustWork = FALSE))
    descriptor_file <- paste0(basename(file), ".desc")
    m <- bigmemory::attach.big.matrix(file.path(backing_dir, descriptor_file))
  } else if (!is.null(mat)) {
    m <- mat
  } else {
    stop("Either 'file' or 'mat' must be provided")
  }

  dim_m <- dim(m)

  # Count data
  ex <- seq(0, 1, length.out = bins + 1)
  px <- numeric(bins)

  for (i in seq(1, dim_m[1], by = batch)) {
    rows <- i:min(i + batch - 1, dim_m[1])
    msub <- m[rows, seq_len(dim_m[2])]
    px <- px + .hist_counts(as.vector(msub), ex)
  }

  list(px = px, ex = ex)
}
