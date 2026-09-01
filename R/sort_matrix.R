#' Sort network nodes by similarity in connectivity
#'
#' Uses PCA to find similarities in the connectivity profiles of nodes,
#' then sorts nodes so that ones associated with the same principal
#' component(s) end up next to each other.
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
#' @section Important: PCA sign ambiguity affects sort order.
#' Principal components are only defined up to an arbitrary sign flip --
#' this is a property of PCA itself, not specific to any one
#' implementation. Since this function ranks each node's components by
#' \emph{signed} score (highest first), a component that comes out
#' sign-flipped between MATLAB's \code{pca()} and R's \code{prcomp()} could
#' genuinely produce a different node ordering in the two languages, even
#' with an entirely correct translation. If exact node-order parity with
#' MATLAB ever matters here, this is the first thing to check --
#' comparing e.g. absolute values or the underlying variance explained,
#' rather than exact order/rank, may be a more meaningful comparison than
#' checking for an identical \code{order} output.
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
