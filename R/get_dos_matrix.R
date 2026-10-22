#' Calculate the Degree of Separation (DOS) matrix of a network
#'
#' Computes the degree of separation (DOS), defined as the minimum number
#' of steps (edges) necessary to travel between two nodes in a network.
#' DOS is the unweighted equivalent of the shortest-path distance, as it
#' considers only the number of edges and not their weights. This function
#' computes the DOS for all pairs of nodes and returns a matrix with the
#' same dimensions as the input matrix.
#'
#' This works by setting every nonzero MI value to exactly \code{0.5}
#' before calling \code{shortestpath()}. Since \code{shortestpath()}
#' computes distance as \code{(1 - mi) / mi}, this makes every real edge
#' cost exactly \code{1}, turning the weighted shortest-path search into a
#' plain hop-count (unweighted) shortest-path search.
#'
#' @param mi MI/network matrix.
#'
#' @return An \code{n x n} DOS matrix, same size as \code{mi}, where entry
#'   \code{[i, j]} is the minimum number of edges on a path from node
#'   \code{i} to node \code{j}.
#'
#' @seealso \code{shortestpath}, \code{kshortestpaths}
#' @family networkanalysis
#' @export
get_dos_matrix <- function(mi) {
  storage.mode(mi) <- "double"
  mi[mi > 0] <- 0.5  # For conversion to distance metric by shortestpath()

  n <- nrow(mi)
  dos <- matrix(0, n, n)

  for (i in seq_len(n)) {
    dos[i, ] <- shortestpath(mi, i)$d
  }

  dos
}
