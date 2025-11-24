#' Run MIMIC Test
#'
#' This function run MIMIC(Multiple-Indicator, Multiple-Cause) test on selected
#' items of a linear model, excluding anchor items. This function reports z-scores
#' and p-scores of group regression given null hypothesis 0.
#'
#' Notice: Can not detect DIF in factor loadings.
#'
#' @param model A `harm_model` object representing model structure
#' @param target_items A vector of names of targeted items
#' @param target_cohorts A vector of names of targeted cohorts
#' @param anchors a vector of names of anchor items
#'
#' @return A table summarize prediction of DIF on targeted items
#' @export


mimic <- function(model, target_items, target_cohorts, anchors) {

    cat("\n--- Running MIMIC Test ---\n")

    # 0. extract info from harm_model
    cohorts <- model$cohorts
    d_t_map <- model$domain_test_map
    c_t_map <- model$cohort_test_map
    group_var <- model$group_var
    dat_tmp <- df_from_data_list(model = model,
                                      target_items = target_items,
                                      target_groups = target_cohorts)


    # 1. Construct the full model syntax
    mimic_syntax <- lvn_syntax(model,
                               target_items,
                               loadings         = NULL,
                               fix_lv_means     = TRUE,
                               fix_lv_variances = TRUE,
                               group_reg        = setdiff(target_items, anchors)
                    )

    # Print the model being fitted for user transparency
    cat("--- Fitting the following MIMIC model ---\n")
    cat(mimic_syntax)
    cat("\n-----------------------------------------\n\n")


    # 2. Fit the MIMIC model
    fit <- lavaan::sem(mimic_syntax, data = dat_tmp)

    # 3. Collect fit indices and p-value

    # fitness indices
    indices <- lavaan::summary(fit, fit.measures=TRUE)$fit[c("cfi","srmr","rmsea")]

    # parameter estimations of targeted items
    pe <- lavaan::parameterEstimates(fit)
    dif_results <- pe[pe$op == "~" & pe$lhs %in% target_items & pe$rhs == group_var,
                      c("lhs", "est", "se", "z", "pvalue")]
    colnames(dif_results) <- c("item", "estimate", "std_error", "z_value", "p_value")

    cat("--- LASSO DIF Detection Results ---\n")
    cat("--- Fitting criterions ---\n\n")
    print(indices)
    cat('\n--- Parameter Estimations --- \n\n')
    print(dif_results)

    return(dif_results)
}
