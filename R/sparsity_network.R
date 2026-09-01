#' Remove indirect links from the network using network sparsification
#'
#' Removes links from the network that are the result of indirect
#' correlations, via sparsification based on the Data Processing
#' Inequality. Iterates over all triangles in the network and removes the
#' weakest link in each. Also used to calculate the network Global
#' Clustering Coefficient (GCC).
#'
#' @param mi Network (MI) matrix. Assumed symmetric.
#'
#' @return A list with:
#'   \item{sps}{Sparsified network matrix.}
#'   \item{gcc}{Global clustering coefficient.}
#'
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
