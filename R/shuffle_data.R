#' Shuffle variables within each sample
#'
#' Randomly permutes the values across variables within each column of the
#' data matrix. Each column is shuffled independently, preserving the set
#' of observed values within each sample while changing their assignment
#' to variables.
#'
#' @param data Data matrix (rows are variables, columns are samples).
#'
#' @return Shuffled data matrix with the same dimensions as \code{data}.
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
