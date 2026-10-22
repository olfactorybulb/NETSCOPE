#' Calculate global clustering coefficient
#'
#' Counts the number of triangles in an undirected network and divides
#' by the total number of possible node triples.
#'
#' @param mi MI/network matrix.
#'
#' @return A numeric scalar giving the global clustering coefficient as
#'   defined in the normalization section below.
#'
#' @section Note on normalization:
#' This function divides the triangle count by \code{choose(n, 3)}, the total
#' number of possible node triples in the network, whether connected or not.
#' This differs from the standard global clustering coefficient (transitivity),
#' which normalizes by the number of connected triples (paths of length 2).
#' Therefore, values returned by this function are not directly comparable
#' with clustering coefficients computed using the standard transitivity
#' definition.
#'
#' The \code{gcc} value returned by \code{sparsify_network()} uses the same
#' definition and is calculated from the original network before sparsification.
#'
#' @seealso \code{get_localcc}, \code{sparsify_network}
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
