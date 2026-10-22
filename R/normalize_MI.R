#' Normalize MI and calculate VI matrix
#'
#' Normalizes an MI matrix with respect to joint entropy, calculates
#' normalized Variation of Information (VI) as \code{1 - MI}, and
#' calculates the distance matrix \code{VI / MI}.
#'
#' @param mi_unnorm Unnormalized MI matrix.
#' @param hi Entropy vector.
#' @param hj Optional entropy vector. Defaults to \code{hi} (symmetric
#'   case), matching the two-argument call used elsewhere (e.g.
#'   \code{compute_MI}).
#'
#' @return A list with:
#'   \item{mi}{Normalized, dimensionless MI matrix.}
#'   \item{dist}{Distance matrix calculated as normalized VI / normalized MI.}
#'
#' @details
#' Mutual information and entropy are calculated using natural logarithms
#' (nats). Normalized MI and the resulting distance matrix are dimensionless
#' and invariant to the choice of logarithm base, provided the same base
#' is used consistently.
#'
#' @export
normalize_MI <- function(mi_unnorm, hi, hj = NULL) {
  if (is.null(hj)) {
    hj <- hi
  }

  # Joint entropy: outer sum of hi and hj, minus the unnormalized MI
  joint_h <- outer(hi, hj, "+") - mi_unnorm

  # Normalized MI
  mi <- mi_unnorm / joint_h
  mi[is.nan(mi)] <- 0

  # Normalized variation of information
  vi <- 1 - mi
  dist <- vi / mi

  list(mi = mi, dist = dist)
}
