#' Compute an MI block for a batch of variable pairs
#'
#' Internal function called by \code{compute_MI_batch()} and
#' \code{compute_MI_PP()}. Computes the joint MI between the variables
#' indexed by \code{ix} and those indexed by \code{jx}.
#'
#' @param data Either a list with elements \code{data}, \code{px}, \code{ex}
#'   (in-memory case, used by \code{compute_MI_batch()}), or a file path
#'   (character string) to an \code{.rds} file containing the same, saved
#'   ahead of time so parallel workers can each load it independently
#'   without needing the full object copied into every worker's memory
#'   (used by \code{compute_MI_PP()}).
#' @param ix Integer vector of row-variable indices.
#' @param jx Integer vector of column-variable indices.
#'
#' @return A list with:
#'   \item{mi}{\code{length(ix)} x \code{length(jx)} MI block.}
#'   \item{ix}{Passed through unchanged, for matching results back to their
#'     block when running asynchronously.}
#'   \item{jx}{Passed through unchanged.}
#'
#' @keywords internal
process_batch <- function(data, ix, jx) {

  if (is.list(data)) {
    px <- data$px
    ex <- data$ex
    data <- data$data
  } else {
    loaded <- readRDS(data)
    px <- loaded$px
    ex <- loaded$ex
    data <- loaded$data
  }

  half <- length(ix) == length(jx) && all(ix == jx)  # along the diagonal

  mi <- matrix(0, nrow = length(ix), ncol = length(jx))

  for (i in seq_along(ix)) {
    if (sum(data[ix[i], ]) == 0) next

    for (j in seq_along(jx)) {
      if ((half && ix[i] <= jx[j]) || (sum(data[jx[j], ]) == 0)) next

      # Compute joint distribution and MI
      pxy <- .hist2d_counts(data[ix[i], ], data[jx[j], ], ex[[ix[i]]], ex[[jx[j]]])
      pxy <- pxy / sum(pxy)

      joint_expected <- outer(px[[ix[i]]], px[[jx[j]]])
      term <- pxy * log(pxy / joint_expected)

      mi[i, j] <- sum(term, na.rm = TRUE)
    }
  }

  if (half) {
    mi <- mi + t(mi)
  }

  list(mi = mi, ix = ix, jx = jx)
}
