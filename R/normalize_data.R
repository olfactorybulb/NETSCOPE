#' Normalize data matrix
#'
#' Normalize data with respect to the sample (column) total.
#'
#' @param data Numeric matrix (rows = variables, columns = samples).
#'
#' @return Normalized data matrix, same dimensions as \code{data}.
#'
#' @details
#' Column-total normalization is intended primarily for count data,
#' such as gene-expression measurements, where differences in column
#' totals may reflect differences in library size or sequencing depth.
#' This normalization is not generally appropriate for psychological
#' scores, such as cognitive test scores or questionnaire responses.
#' Columns with a total of zero produce \code{NaN} values.
#'
#' @export
normalize_data <- function(data) {
  sweep(data, 2, colSums(data), "/")
}
