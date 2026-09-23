#' Calculate K shortest paths in a graph using Yen's algorithm
#'
#' Computes up to \code{K} loopless shortest paths between a source and
#' target node using Yen's algorithm. See \code{shortestpath()} for details
#' on how path lengths and individual shortest paths are computed.
#'
#' @param mi MI/network matrix.
#' @param K Maximum number of shortest paths to find.
#' @param source Source node index (1-based).
#' @param target Target node index (1-based, single node).
#'
#' @return A list with:
#'   \item{d}{Numeric vector containing the path lengths for each of the
#'     (up to) \code{K} paths found.}
#'   \item{A}{List of (up to) \code{K} integer vectors, each representing
#'     a path from \code{source} to \code{target}. If fewer than \code{K}
#'     loopless paths exist, remaining entries are \code{NULL}.}
#'
#' @seealso \code{shortestpath}
#' @family networkanalysis
#' @export
kshortestpaths <- function(mi, K, source, target) {
  A <- vector("list", K)
  first <- shortestpath(mi, source, target)  # First KSP using Dijkstra
  A[[1]] <- first$paths[[1]]

  B <- list()        # Potential KSPs
  Bc <- numeric(0)    # Cost of potential KSPs

  for (k in seq_len(K - 1)) {
    path_k <- A[[k]]
    for (i in seq_len(length(path_k) - 1)) {
      mi1 <- mi                     # Copy of original MI matrix
      spurNode <- path_k[i]         # Set the spur node
      rootPath <- path_k[1:i]       # Copy root path from previous KSP
      rootCost <- get_pathlength(mi1, rootPath)  # Cost of root path

      for (j in seq_len(k)) {       # For all KSPs already found
        p <- A[[j]]
        if (length(p) > i && identical(rootPath, p[1:i])) {
          mi1[p[i], p[i + 1]] <- 0  # Remove edge from graph
        }
      }

      # Remove root path nodes (except the spur node itself) from the graph
      trim_idx <- head(rootPath, -1)
      mi1[trim_idx, ] <- 0
      mi1[, trim_idx] <- 0

      spur <- shortestpath(mi1, spurNode, target)
      spurCost <- spur$d
      spurPath <- spur$paths[[1]]

      totalPath <- c(rootPath, tail(spurPath, -1))
      totalCost <- rootCost + spurCost

      B[[length(B) + 1]] <- totalPath
      Bc <- c(Bc, totalCost)

      # Discard path if duplicate
      for (j in seq_len(length(B) - 1)) {
        if (identical(B[[j]], totalPath)) {
          B[[length(B)]] <- NULL
          Bc <- Bc[-length(Bc)]
          break
        }
      }
    }

    ksp <- which.min(Bc)
    if (length(B) == 0 || tail(B[[ksp]], 1) != target) {
      message(sprintf("Found %d paths.", k + 1))
      break
    } else {
      A[[k + 1]] <- B[[ksp]]
    }
    B[[ksp]] <- NULL
    Bc <- Bc[-ksp]
  }

  # MATLAB transposes A from a Kx1 to a 1xK cell array here purely for
  # matrix-shape bookkeeping. A flat R list has no row/column shape to fix,
  # so there's nothing to do at this step.
  d <- get_pathlength(mi, A)

  list(d = d, A = A)
}
