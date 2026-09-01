#' Compute entropy from data distributions
#'
#' @param px List of per-variable probability distributions, as produced by
#'   \code{get_distributions()}.
#'
#' @return Numeric vector with the entropy of each variable.
#'
#' @seealso get_distributions
#'
#' @export
get_entropy <- function(px) {
  nvars <- length(px)
  h <- numeric(nvars)

  for (i in seq_len(nvars)) {
    if (is.null(px[[i]])) next
    h[i] <- -sum(px[[i]] * log(px[[i]]), na.rm = TRUE)
  }

  h
}
