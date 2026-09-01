#' Calculate local clustering coefficient for every node in network
#'
#' For each node, measures how interconnected its neighbors are with each
#' other, relative to the maximum possible number of connections among
#' them.
#'
#' @param mi MI/network matrix.
#'
#' @return Numeric vector of length \code{n}, the local clustering
#'   coefficient for each node. Nodes with 0 or 1 neighbors have an
#'   undefined (0/0) coefficient, reported as \code{0}.
#'
#' @seealso \code{get_globalcc}, \code{get_centrality}
#' @family networkanalysis
#' @export
get_localcc <- function(mi) {
  mi <- mi > 0
  diag(mi) <- FALSE

  n <- nrow(mi)
  lcc <- numeric(n)

  for (i in seq_len(n)) {
    nb <- which(mi[i, ])            # Node neighbors
    nhs <- length(nb)               # Neighborhood size
    nbh <- mi[nb, nb, drop = FALSE] # Neighborhood map
    lcc[i] <- sum(nbh) / (nhs * (nhs - 1))
  }

  lcc[is.nan(lcc)] <- 0  # If a node has no neighbours, LCC is undefined
  lcc
}
