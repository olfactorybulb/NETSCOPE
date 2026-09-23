#' Compute entropy from probability distributions
#'
#' Computes the Shannon entropy of each variable from its probability
#' distribution. Entropy is calculated using the natural logarithm and is
#' therefore expressed in nats.
#'
#' @param px List of per-variable probability distributions, as produced by
#'   \code{get_distributions()}.
#'
#' @return Numeric vector containing the entropy of each variable.
#'
#' @seealso \code{get_distributions}
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
