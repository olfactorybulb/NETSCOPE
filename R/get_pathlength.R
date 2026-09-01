#' Calculate the pathlength of a path (or paths) in a network
#'
#' The pathlength is the sum of the distances between the nodes along the
#' path. Distance is the ratio of Variation of Information (VI,
#' \code{1 - mi}) to Mutual Information (\code{mi}).
#'
#' @param mi MI/network matrix.
#' @param p Either a single path (integer vector of node indices), or a
#'   list of paths. List entries may be \code{NULL} (e.g. unused/unfound
#'   paths from \code{kshortestpaths()}) -- these contribute a length of 0.
#'
#' @return If \code{p} was a single path, a single number. If \code{p} was
#'   a list of paths, a numeric vector of pathlengths, one per path.
#'
#' @section Note:
#' Unlike \code{shortestpath()}, this function does NOT clamp \code{mi}
#' values above 1 down to 1 before computing distances. If \code{mi} can
#' ever contain values slightly over 1 (e.g. from normalization rounding),
#' this could disagree slightly with distances computed inside
#' \code{shortestpath()}/\code{kshortestpaths()} for the same path. Ported
#' as-is from source; flagging in case this should be reconciled.
#'
#' @seealso \code{shortestpath}, \code{kshortestpaths}
#' @family networkanalysis
#' @export
get_pathlength <- function(mi, p) {
  dist <- (1 - mi) / mi

  was_list <- is.list(p)
  if (!was_list) p <- list(p)

  d <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[[i]]
    if (is.null(pi) || length(pi) < 2) next  # stays 0, matches MATLAB's empty-path behavior
    for (j in 2:length(pi)) {
      d[i] <- d[i] + dist[pi[j - 1], pi[j]]
    }
  }

  if (!was_list) d <- d[[1]]
  d
}
