#' Compute mutual information (MI) matrix from continuous data
#'
#' Computes an MI matrix where each entry (i, j) is the mutual information
#' between variables i and j. Intended for datasets small enough to fit in
#' memory, where no parallel/batch processing is needed.
#'
#' @param data Numeric matrix (rows = variables, columns = samples). Required.
#' @param px Optional list of marginal distributions (one numeric vector per
#'   variable), as produced by \code{get_distributions()}. Computed if not
#'   supplied.
#' @param ex Optional list of bin edges (one numeric vector per variable).
#'   Required if \code{px} is supplied.
#' @param h Optional numeric vector of per-variable entropies, as produced by
#'   \code{get_entropy()}. Computed if not supplied.
#'
#' @return A symmetric numeric matrix containing pairwise MI values,
#'   normalized with respect to joint entropy.
#'
#' @seealso compute_MI_batch, normalize_MI
#'
#' @examples
#' # Create a small example dataset with variables in rows and samples in columns
#' set.seed(123)
#' data <- matrix(rnorm(300), nrow = 3)
#'
#' # Compute the MI matrix
#' mi <- compute_MI(data)
#' mi
#'
#' # Alternatively, supply precomputed distributions and entropies
#' distributions <- get_distributions(data)
#' h <- get_entropy(distributions$px)
#' mi <- compute_MI(
#'   data,
#'   px = distributions$px,
#'   ex = distributions$ex,
#'   h = h
#' )
#'
#' @export
compute_MI <- function(data, px = NULL, ex = NULL, h = NULL) {

  ## Parse arguments, compute px, h if necessary -----------------------------
  if (is.null(px)) {
    distributions <- get_distributions(data)
    px <- distributions$px
    ex <- distributions$ex
  }
  if (is.null(h)) {
    h <- get_entropy(px)
  }

  ## Compute MI matrix ---------------------------------------------------------
  nvars <- length(px)
  mi <- matrix(0, nrow = nvars, ncol = nvars)

  for (i in seq_len(nvars)) {
    if (sum(data[i, ]) == 0) next

    for (j in seq_len(i - 1)) {
      if (sum(data[j, ]) == 0) next

      # Joint distribution and MI contribution
      pxy <- .hist2d_counts(data[i, ], data[j, ], ex[[i]], ex[[j]])
      pxy <- pxy / sum(pxy)

      joint_expected <- outer(px[[i]], px[[j]])
      term <- pxy * log(pxy / joint_expected)

      mi[i, j] <- sum(term, na.rm = TRUE)
    }
  }

  ## Normalize and symmetrize MI matrix ----------------------------------------
  mi <- normalize_MI(mi + t(mi), h)$mi
  mi
}
