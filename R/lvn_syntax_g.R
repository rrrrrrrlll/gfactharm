#' Generate `lavaan` Syntax of First Order Linear Model
#'
#' Given full model information, generate model syntax of a 1st order linear model
#' include only selected items.
#'
#' @param model A `harm_model` object representing model structure.
#' @param selected_items A character vector of names of selected items.
#'
#' @param loadings A named numeric vector. Names are selected item names, values
#'  are the loadings to fix. Items not in this vector will be freely estimated.
#'  If `NULL` (default), all loadings are freely estimated.
#' @param fix_lv_means Logical.
#'  If `TRUE`, latent variable intercepts are fixed (see `fixed_intercept`).
#'  If `FALSE` (default), intercepts are freely estimated.
#' @param fix_lv_variances Logical.
#'  If `TRUE`, latent variable variances are fixed (see `fixed_residual_var`).
#'  If `FALSE` (default), variances are freely estimated.
#' @param fixed_intercept The numeric value to fix item intercepts to when
#'  `fix_means = TRUE`. Default is `0.0`.
#' @param fixed_variance The numeric value to fix item residual variances
#'  to when `fix_lv_variances = TRUE`. Default is `1.0`.
#' @param group_reg A character vector of item names.
#'  If non-empty, items in it will add a regression to the group variable.
#'  If `NULL` (default), skip this part.
#'
#' @return A character string of the formatted lavaan model syntax.


lvn_syntax <- function(model,
                       selected_items,
                       loadings         = NULL,
                       fix_lv_means     = FALSE,
                       fix_lv_variances = FALSE,
                       fixed_intercept  = 0.0,
                       fixed_variance   = 1.0,
                       group_reg        = NULL) {

}
