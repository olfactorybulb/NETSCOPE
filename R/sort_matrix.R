#' Sort network nodes by similarity in connectivity
#'
#' Uses PCA to identify similarities in the connectivity profiles of nodes,
#' then sorts nodes so that those associated with the same principal
#' component(s) are placed next to each other.
#'
#' @param mi MI/network matrix (\code{N} by \code{N}).
#' @param ncomps Optional number of components to use (to refine sorting).
#'   Defaults to the maximum, \code{nconn - 1}, where \code{nconn} is the
#'   number of nodes with at least one nonzero connection.
#'
#' @return A list with:
#'   \item{order}{New order of the nodes, so that \code{mi[order, order]}
#'     is the sorted matrix.}
#'   \item{max_comp}{An \code{N} by \code{ncomps} matrix; row \code{i}
#'     gives, for node \code{i}, the component indices it scores highest
#'     on, ranked highest first. Disconnected nodes get a row of \code{NA}
#'     and sort to the end.}
#'
#' @section PCA sign ambiguity:
#' Principal components are defined only up to an arbitrary sign flip.
#' Because this function ranks components using signed PCA scores, the
#' resulting node order may differ across PCA implementations even when
#' the underlying PCA solutions are equivalent. Therefore, exact node
#' ordering may not be reproducible across different PCA implementations.
#'
#' @family networkanalysis
#' @export
sort_matrix <- function(mi, ncomps = NULL) {
  storage.mode(mi) <- "double"
  n <- nrow(mi)
  conn <- which(colSums(mi) != 0)
  nconn <- length(conn)

  if (is.null(ncomps)) {
    ncomps <- nconn - 1
  }

  max_comp <- matrix(NA_real_, n, ncomps)

  pca_res <- stats::prcomp(mi[conn, conn, drop = FALSE], center = TRUE, scale. = FALSE)
  scores <- pca_res$x

  for (i in seq_len(nconn)) {
    ord <- order(scores[i, ], decreasing = TRUE)
    max_comp[conn[i], ] <- ord[1:ncomps]
  }

  order_result <- do.call(order, as.data.frame(max_comp))

  list(order = order_result, max_comp = max_comp)
}
