#' Remove potentially indirect links using network sparsification
#'
#' Applies a Data Processing Inequality (DPI)-based heuristic to sparsify
#' a weighted MI network. For each triangle, the weakest edge is removed
#' when it is strictly weaker than the other two edges.
#' Also calculates the global clustering coefficient of the original network.
#'
#' @param mi Network (MI) matrix. Assumed symmetric.
#'
#' @return A list with:
#'   \item{sps}{Sparsified network matrix.}
#'   \item{gcc}{Triangle count of the original network divided by
#'     \code{choose(n, 3)}, consistent with \code{get_globalcc()}.}
#'
#' @details
#' All triangles are evaluated using the original MI matrix, rather than
#' the progressively sparsified network. Edge removal therefore does not
#' affect subsequent triangle evaluations.
#'
#' An edge is removed only when its weight is strictly smaller than both
#' other edges in a triangle. Ties for the weakest edge are retained.
#' No tolerance is applied when comparing edge weights.
#'
#' @seealso \code{get_globalcc}
#' @export
sparsify_network <- function(mi) {

  sps <- mi
  sps[!lower.tri(sps)] <- 0  # strictly lower triangular, like tril(mi, -1)

  idx <- which(sps > 0, arr.ind = TRUE)
  s <- idx[, "row"]
  tgt <- idx[, "col"]  # s > tgt always, since sps is strictly lower triangular

  gcc <- 0

  for (i in seq_along(s)) {
    # Edge triangular neighbors
    u <- which(mi[s[i], 1:tgt[i]] > 0 & mi[tgt[i], 1:tgt[i]] > 0)
    u <- u[!(u == s[i] | u == tgt[i])]  # don't double count edge i

    gcc <- gcc + length(u)

    for (j in seq_along(u)) {
      if (mi[s[i], tgt[i]] < mi[s[i], u[j]] && mi[s[i], tgt[i]] < mi[tgt[i], u[j]]) {
        sps[s[i], tgt[i]] <- 0
      } else if (mi[tgt[i], u[j]] < mi[s[i], u[j]] && mi[tgt[i], u[j]] < mi[s[i], tgt[i]]) {
        sps[tgt[i], u[j]] <- 0
      } else if (mi[s[i], u[j]] < mi[s[i], tgt[i]] && mi[s[i], u[j]] < mi[tgt[i], u[j]]) {
        sps[s[i], u[j]] <- 0
      }
    }
  }

  sps <- sps + t(sps)
  gcc <- gcc / choose(nrow(mi), 3)

  list(sps = sps, gcc = gcc)
}
