#' Gini correlation
#'
#' This function calculates the Gini correlation between two distributions.
#' @param x a numeric vector containing at least non-negative elements.
#' @param y a numeric vector containing the distribution with the rank information.
#' @param weights an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @return The value of the Gini correlation, should be between -1 and 1.
#' @export

gini_corr <- function(x, y, weights = NULL) {
  gini_wtd_ord(x, y, weights) / gini_wtd_ord(x, weights = weights)
}
