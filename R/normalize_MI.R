#' Normalize MI and calculate VI matrix
#'
#' Normalizes an MI matrix with respect to the joint entropy, calculates
#' the Variation of Information (VI) matrix (which measures information
#' distance, as opposed to MI which measures information similarity), and
#' calculates the distance matrix VI / MI.
#'
#' @param mi_unnorm Unnormalized MI matrix.
#' @param hi Entropy vector.
#' @param hj Optional entropy vector. Defaults to \code{hi} (symmetric
#'   case), matching the two-argument call used elsewhere (e.g.
#'   \code{compute_MI}).
#'
#' @return A list with:
#'   \item{mi}{Normalized MI matrix.}
#'   \item{dist}{Normalized VI distance matrix (\code{VI / MI}).}
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

  # Variation of information; a distance measure
  vi <- 1 - mi  # == joint_h - mi_unnorm
  dist <- vi / mi

  list(mi = mi, dist = dist)
}
