#' Calculate global clustering coefficient
#'
#' Counts triangles in the network (each triangle counted exactly once, via
#' the edge connecting its two largest-indexed nodes, searching for the
#' third/smallest-indexed node as a shared neighbor), then normalizes by
#' the total number of possible node triples.
#'
#' @param mi MI/network matrix.
#'
#' @return A single number, the global clustering coefficient.
#'
#' @section Note on normalization:
#' This divides the triangle count by \code{choose(n, 3)} -- the total
#' number of possible node triples in the network, whether connected or
#' not. The standard "global clustering coefficient" / transitivity
#' formula in the literature instead divides by the number of *connected*
#' triples (paths of length 2), which is a different quantity. Ported
#' as-is from source; the resulting value won't be directly comparable to
#' clustering coefficients reported by other tools using the standard
#' formula.
#'
#' @seealso \code{get_localcc}
#' @family networkanalysis
#' @export
get_globalcc <- function(mi) {
  mi[upper.tri(mi)] <- 0  # tril(), diagonal kept
  idx <- which(mi > 0, arr.ind = TRUE)
  s <- idx[, 1]  # Edge source indices (s > t)
  t <- idx[, 2]  # Edge target indices

  gcc <- 0
  for (i in seq_along(s)) {
    si <- s[i]
    ti <- t[i]
    cols <- seq_len(ti)
    # Edge triangular neighbors
    u <- cols[mi[si, cols] > 0 & mi[ti, cols] > 0]
    u <- u[u != si & u != ti]  # Don't double count edge i
    gcc <- gcc + length(u)
  }

  gcc / choose(nrow(mi), 3)
}
