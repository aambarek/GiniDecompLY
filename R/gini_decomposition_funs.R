#' Gini decomposition by income sources
#'
#' This function provides a decomposition of Gini index by income sources based on the approach of Lerman and Yitzhaki (1985).
#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @return A tibble containing contribution of each source to the income inequality.
#' @export
#'


gini_decomp_source <- function(.data, ..., .by = NULL, .wgt = NULL) {

  decomp_components <- gini.source.decomp.comp(.data, ..., .by = {{.by}}, .wgt = {{.wgt}})

  decomp_results <- decomp_components %>%
    dplyr::group_by(dplyr::across({{.by}})) %>%
    dplyr::mutate(Absolute_Contribution = Share * Gini * Gini_corr,
                  Relative_Contribution = Absolute_Contribution / sum(Absolute_Contribution))

  return(decomp_results)

}


#' Gini income elasticity
#'
#' This function computes the elasticity of Gini index associated with a percentage change in the mean income (for each income source).
#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @return A tibble containing the value of elasticity and the marginal effect of each source on income inequality.
#' @export
#'


gini_income_elasticity <- function(.data, ..., .by = NULL, .wgt = NULL) {

  decomp_components <- gini.source.decomp.comp(.data, ..., .by = {{.by}}, .wgt = {{.wgt}})

  gini_income_elasticity <- decomp_components %>%
    dplyr::group_by(dplyr::across({{.by}})) %>%
    dplyr::mutate(Elasticity = Gini * Gini_corr / sum(Share * Gini * Gini_corr),
                  Marginal_Impact = Share * (Elasticity - 1))

  return(gini_income_elasticity)
}


#' Growth-redistribution impacts on social welfare function.
#'
#' This function provides a Growth-redistribution decomposition of effects (for each income source) on social welfare function defined by Amartya Sen (1970).
#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @return A tibble containing growth effect, redistribution effect, and overall effect of each income source.
#' @export
#'


social_welfare_impact <- function(.data, ..., .by = NULL, .wgt = NULL) {

  decomp_components <- gini.source.decomp.comp(.data, ..., .by = {{.by}}, .wgt = {{.wgt}})

  social_welfare_impact <- decomp_components %>%
    dplyr::group_by(dplyr::across({{.by}})) %>%
    dplyr::mutate(Growth_Effect = Share / (1 - sum(Share * Gini * Gini_corr)),
                  Redistribution_Effect = - Share * Gini * Gini_corr / (1 - sum(Share * Gini * Gini_corr)),
                  Total_Variation = Growth_Effect + Redistribution_Effect)

  return(social_welfare_impact)
}
