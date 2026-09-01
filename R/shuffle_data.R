#' Column-wise shuffle the data matrix
#'
#' Shuffle each column of the data matrix to remove any correlations between
#' variables but preserve the sample-specific profiles.
#'
#' @param data Data matrix (rows are variables, columns are samples).
#'
#' @return Shuffled data matrix, same dimensions as \code{data}.
#'
#' @family expressiondata
#' @export
shuffle_data <- function(data) {
  shuffled <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  for (i in seq_len(ncol(data))) {
    shuffled[, i] <- data[sample(nrow(data)), i]
  }
  shuffled
}
