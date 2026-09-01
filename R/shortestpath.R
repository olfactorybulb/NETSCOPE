#' Calculate the shortest path in a graph using Dijkstra's algorithm
#'
#' Calculates the shortest path from a source node to a target node (or all
#' other nodes) in the network. The length of a path is the sum of the
#' distances between the nodes along the path. The shortest path is the one
#' minimizing this total length. Distance between nodes is defined as the
#' ratio of Variation of Information (VI, \code{1 - mi}) to Mutual
#' Information (\code{mi}).
#'
#' @param mi MI/network matrix.
#' @param source Source node index (1-based).
#' @param target Optional target node index (1-based). When left at its
#'   default (\code{-1}, which never matches a real node index), the
#'   shortest path from \code{source} to every other node is computed
#'   instead of stopping at a single target — mirroring MATLAB's
#'   \code{nargin == 2} branch.
#'
#' @return A list with:
#'   \item{d}{Path length(s) of the shortest path(s). A single number if
#'     \code{target} was reached and matched, or a length-\code{n} vector
#'     of distances to every node otherwise.}
#'   \item{paths}{A list of integer vectors, each giving the sequence of
#'     node indices (including both endpoints) on a shortest path. Length 1
#'     if a single \code{target} was matched; length \code{n} (one path per
#'     node) otherwise.}
#'
#' @seealso \code{get_pathlength}, \code{kshortestpaths}, \code{get_dos_matrix}
#' @family networkanalysis
#' @export
shortestpath <- function(mi, source, target = -1) {
  mi[mi > 1] <- 1
  vi <- (1 - mi) / mi
  n <- nrow(vi)

  d <- rep(Inf, n)           # distance between source and targets
  d[source] <- 0
  prev <- rep(-1L, n)        # previous node in shortest path, -1 = undefined
  q <- rep(TRUE, n)          # unvisited nodes
  nodes <- seq_len(n)        # list of all nodes

  while (any(q)) {
    # Select closest unvisited node
    dq <- d[q]
    k <- which.min(dq)
    un <- nodes[q]            # all unvisited nodes
    u <- un[k]                # closest unvisited node
    q[u] <- FALSE              # node is visited

    # If target node is reached, quit
    if (u == target) {
      d_out <- d[u]
      p <- integer(0)
      uu <- u
      while (uu != -1) {
        p <- c(uu, p)
        uu <- prev[uu]
      }
      return(list(d = d_out, paths = list(p)))
    }

    # For every target: if the new path (via u) is shorter than the old
    # one, replace. (Not equivalent to checking vi(u,:) > 0, due to Inf.)
    tdist <- d[u] + vi[u, ]
    index <- tdist < d
    d[index] <- tdist[index]
    prev[index] <- u
  }

  # Calculate paths to every node
  paths <- vector("list", n)
  for (i in seq_len(n)) {
    u <- i
    p <- integer(0)
    while (u != -1) {
      p <- c(u, p)
      u <- prev[u]
    }
    paths[[i]] <- p
  }

  list(d = d, paths = paths)
}
