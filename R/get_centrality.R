#' Get network betweenness centrality
#'
#' For every node, calculate the "betweenness centrality": a measure of how
#' important a node's role is in the global network structure, based on how
#' often it lies on the shortest path between other pairs of nodes.
#'
#' @param mi MI/network matrix.
#' @param paths Optional nested list of shortest paths, where
#'   \code{paths[[i]][[j]]} is an integer vector giving the sequence of node
#'   indices (including both endpoints) on the shortest path from node
#'   \code{i} to node \code{j}. Computed via \code{shortestpath()} if
#'   missing.
#'
#' @return Numeric vector of length \code{n} (where \code{n = nrow(mi)})
#'   giving the normalized betweenness centrality of each node.
#'
#' @section On \code{igraph}:
#' This is algorithmically equivalent to \code{igraph::betweenness()}, but
#' is NOT swapped in here, since the edge-weight convention
#' \code{shortestpath()} uses to turn MI values into path "distances" isn't
#' known yet (e.g. whether it inverts or log-transforms MI). Revisit once
#' \code{shortestpath.m} has been reviewed.
#'
#' @seealso \code{shortestpath}, \code{get_localcc}
#' @family networkanalysis
#' @export
get_centrality <- function(mi, paths = NULL) {
  n <- nrow(mi)

  if (is.null(paths)) {
    message("Computing shortest paths...")
    paths <- vector("list", n)
    for (i in seq_len(n)) {
      paths[[i]] <- shortestpath(mi, i)$paths
    }
  }

  ct <- numeric(n)
  message("Computing betweenness centrality...")
  for (i in seq_len(n)) {
    if (i > 1) {
      for (j in seq_len(i - 1)) {
        p <- paths[[i]][[j]]
        # Guard against R's colon operator: unlike MATLAB's 2:1 (empty),
        # R's 2:1 returns c(2, 1). Only extract interior nodes when a path
        # of length > 2 actually has any.
        if (length(p) > 2) {
          interior <- p[2:(length(p) - 1)]
          ct[interior] <- ct[interior] + 1
        }
      }
    }
  }

  ct <- ct / (0.5 * (n^2 - n))  # Normalize
  ct
}
