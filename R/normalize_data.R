#' Normalize data matrix
#'
#' Normalize data with respect to the sample (column) total.
#'
#' @param data Numeric matrix (rows = variables, columns = samples).
#'
#' @return Normalized data matrix, same dimensions as \code{data}.
#'
#' @export
normalize_data <- function(data) {
  sweep(data, 2, colSums(data), "/")
}
