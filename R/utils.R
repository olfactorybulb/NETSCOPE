## Internal helper functions and shared state for the NETSCOPE package.
## Nothing in this file is exported; everything here is called by other
## package functions internally. Names are prefixed with "." to signal
## they're private and reduce the risk of name collisions with functions
## added later.

#' 1D histogram counts on fixed bin edges (histcounts equivalent)
#'
#' Replicates MATLAB's \code{histcounts(x, edges)} behavior for explicit,
#' pre-specified bin edges: bins are half-open \code{[edge, edge)} except
#' the final bin, which is closed on both ends. Points falling outside
#' \code{[min(edges), max(edges)]} are EXCLUDED, not clamped.
#'
#' @param x Numeric vector of samples.
#' @param edges Numeric vector of bin edges.
#'
#' @return An integer vector of length \code{length(edges) - 1}.
#'
#' @keywords internal
.hist_counts <- function(x, edges) {
  nb <- length(edges) - 1
  idx <- findInterval(x, edges, rightmost.closed = TRUE)
  idx <- idx[idx >= 1 & idx <= nb]
  tabulate(idx, nbins = nb)
}

#' 2D histogram counts on fixed bin edges (histcounts2 equivalent)
#'
#' Replicates MATLAB's \code{histcounts2(x, y, ex, ey)} behavior for
#' explicit, pre-specified bin edges: bins are half-open \code{[edge, edge)}
#' except the final bin, which is closed on both ends. Points falling
#' outside \code{[min(edges), max(edges)]} are EXCLUDED (not clamped into
#' the nearest bin), matching MATLAB's behavior for explicit edge input.
#'
#' @param x,y Numeric vectors of equal length (paired samples).
#' @param ex,ey Numeric vectors of bin edges for x and y respectively.
#'
#' @return A \code{(length(ex)-1) x (length(ey)-1)} matrix of counts.
#'
#' @keywords internal
.hist2d_counts <- function(x, y, ex, ey) {
  nbx <- length(ex) - 1
  nby <- length(ey) - 1

  xi <- findInterval(x, ex, rightmost.closed = TRUE)
  yi <- findInterval(y, ey, rightmost.closed = TRUE)

  # keep only pairs where BOTH coordinates fall inside their edge range
  keep <- xi >= 1 & xi <= nbx & yi >= 1 & yi <= nby
  idx <- (yi[keep] - 1) * nbx + xi[keep]

  counts <- tabulate(idx, nbins = nbx * nby)
  matrix(counts, nrow = nbx, ncol = nby)
}

#' Shared state for parallel MI batch processing
#'
#' Internal environment used to pass job/output state between
#' \code{compute_MI_PP()} and \code{fetch_results()}, mirroring the MATLAB
#' \code{global jobs outfile tempfile} pattern. Not intended for direct use.
#'
#' @keywords internal
.netscope_pp_state <- new.env(parent = emptyenv())
