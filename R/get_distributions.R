#' Compute probability distributions from continuous data
#'
#' Computes the data distribution per variable across samples, by binning
#' and counting the levels. Bins are determined by either Sturges' rule or
#' the Freedman-Diaconis rule. The distributions are used downstream in
#' entropy and mutual-information computations.
#'
#' @param data Numeric matrix (rows = variables, columns = samples).
#' @param binning Which binning rule to use: \code{"freedman"} (default) or
#'   \code{"sturges"}.
#' @param log Whether to log-transform the data (\code{log(data + 1)})
#'   before binning. Default \code{FALSE}.
#'
#' @return A list with:
#'   \item{px}{List of per-variable probability distributions (one numeric
#'     vector per variable).}
#'   \item{ex}{List of per-variable bin edges.}
#'
#' @details
#' The Freedman-Diaconis bin width uses a MATLAB-compatible IQR
#' (\code{.iqr_matlab()}, quantile type 5) rather than base R's
#' \code{stats::IQR()} (type 7), to maintain numerical compatibility with
#' the MATLAB implementation of NETSCOPE.
#'
#' @seealso get_entropy
#'
#' @export
get_distributions <- function(data, binning = "freedman", log = FALSE) {
  if (log) {
    data <- log(data + 1)
  }
  nvars <- nrow(data)
  bin_factor <- ncol(data)^(1 / 3)
  px <- vector("list", nvars)
  ex <- vector("list", nvars)
  for (i in seq_len(nvars)) {
    if (sum(data[i, ]) == 0) next
    if (grepl("freedman", tolower(binning))) {
      # Use MATLAB-compatible IQR (quantile type 5) for numerical consistency.
      iqr_value <- .iqr_matlab(data[i, ])
      bin_width <- 2 * iqr_value / bin_factor
      # Handle edge cases
      if (bin_width <= 0) {
        bin_width <- diff(range(data[i, ])) / 10
      }
      if (bin_width <= 0) {
        bin_width <- 2
      }
      # Create bin edges (note: like MATLAB's colon operator, this may stop
      # short of max_edge if bin_width doesn't evenly divide the range)
      min_edge <- min(data[i, ])
      max_edge <- max(data[i, ])
      ex[[i]] <- seq(min_edge, max_edge, by = bin_width)
    } else {
      # Sturges' rule for bin width and number
      nbins <- ceiling(1 + log2(ncol(data)))
      ex[[i]] <- seq(min(data[i, ]), max(data[i, ]), length.out = nbins + 1)
    }
    # Calculate distributions
    px[[i]] <- .hist_counts(data[i, ], ex[[i]])
    px[[i]] <- px[[i]] / sum(px[[i]])
    px[[i]][is.nan(px[[i]])] <- 0
  }
  if (log) {
    ex <- lapply(ex, function(e) if (is.null(e)) e else exp(e) - 1)
  }
  list(px = px, ex = ex)
}
