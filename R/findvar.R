#' Find variables by name in a list of names
#'
#' Find variables by matching (parts of) names, case-insensitively, and
#' return the results as list indices.
#'
#' @param vars Character vector of names.
#' @param ... Search terms (character strings). Any number of terms may be
#'   passed, either as separate arguments or as character vectors bundling
#'   several terms together (the R equivalent of MATLAB's single-string vs.
#'   cell-array-of-strings distinction) -- all terms are flattened and
#'   matched the same way.
#'
#' @return Integer vector of indices into \code{vars} where the name
#'   contains at least one of the search terms as a substring.
#'
#' @examples
#' vars <- c("alpha", "beta", "alphabet", "gamma")
#' findvar(vars, "alpha")
#' findvar(vars, "alpha", "gamma")
#'
#' @family other
#' @export
findvar <- function(vars, ...) {
  vars <- tolower(vars)
  terms <- tolower(unlist(list(...)))

  found <- rep(FALSE, length(vars))
  for (term in terms) {
    found <- found | grepl(term, vars, fixed = TRUE)  # literal substring match, not regex
  }

  which(found)
}
