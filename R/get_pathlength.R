#' Calculate the pathlength of a path (or paths) in a network
#'
#' The pathlength is the sum of the distances between the nodes along the
#' path. Distance is the ratio of Variation of Information (VI,
#' \code{1 - mi}) to Mutual Information (\code{mi}).
#'
#' @param mi MI/network matrix.
#' @param p Either a single path (integer vector of node indices), or a
#'   list of paths. List entries may be \code{NULL} (e.g., unused or
#'   unavailable paths from \code{kshortestpaths()}); these contribute
#'   a length of 0.
#'
#' @return If \code{p} was a single path, a single number. If \code{p} was
#'   a list of paths, a numeric vector of pathlengths, one per path.
#'
#' @details
#' MI values are clamped to the interval \code{[0, 1]} before
#' calculating distances, consistent with \code{shortestpath()}.
#' Zero MI values correspond to infinite distances.
#'
#' @seealso \code{shortestpath}, \code{kshortestpaths}
#' @family networkanalysis
#' @export
get_pathlength <- function(mi, p) {
  dist <- .mi_to_distance(mi)

  was_list <- is.list(p)
  if (!was_list) p <- list(p)

  d <- numeric(length(p))
  for (i in seq_along(p)) {
    pi <- p[[i]]
    if (is.null(pi) || length(pi) < 2) next  # Empty paths have length 0
    for (j in 2:length(pi)) {
      d[i] <- d[i] + dist[pi[j - 1], pi[j]]
    }
  }

  if (!was_list) d <- d[[1]]
  d
}
