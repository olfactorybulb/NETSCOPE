#' Shuffle data for permutation-based analyses
#'
#' Randomly permutes values within each variable or sample of a data
#' matrix. By default, observations are shuffled independently within
#' each variable (row), preserving each variable's marginal distribution
#' while disrupting the alignment between variables across samples.
#'
#' @param data Numeric matrix (rows = variables, columns = samples).
#' @param margin Character string specifying the permutation method.
#'   \code{"variables"} (default) shuffles observations independently
#'   within each variable (row), preserving variable-wise marginal
#'   distributions. \code{"samples"} shuffles values across variables
#'   independently within each sample (column), preserving the set of
#'   values within each sample.
#'
#' @return Shuffled data matrix with the same dimensions as \code{data}.
#'
#' @details
#' Within-variable permutation is commonly used to construct null
#' distributions for mutual information, as it preserves the marginal
#' distribution of each variable while disrupting cross-variable
#' associations.
#'
#' Within-sample permutation assumes that variables are exchangeable
#' and may be inappropriate when variables are measured on different
#' scales, such as age, cognitive scores, and questionnaire ratings.
#'
#' @examples
#' x <- matrix(1:12, nrow = 3)
#'
#' set.seed(123)
#' shuffle_data(x)
#'
#' set.seed(123)
#' shuffle_data(x, margin = "samples")
#'
#' @family expressiondata
#' @export
shuffle_data <- function(data, margin = c("variables", "samples")) {
  margin <- match.arg(margin)

  shuffled <- data

  if (margin == "variables") {
    # Permute samples independently within each variable
    for (i in seq_len(nrow(data))) {
      shuffled[i, ] <- data[i, sample.int(ncol(data))]
    }
  } else {
    # Permute variables independently within each sample (MATLAB behavior)
    for (i in seq_len(ncol(data))) {
      shuffled[, i] <- data[sample.int(nrow(data)), i]
    }
  }

  shuffled
}
