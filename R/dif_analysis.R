#' Run Exploratory DIF Analysis
#'
#' This function run LASSO regularization test on every item of a linear model,
#' predict uniform DIF based on log-scale greatest gap method.
#' In each domain, the item with least DIF statistic will be selected as anchor,
#' the function will run MIMIC test on remaining items.
#'
#' @param model A `harm_model` object representing model structure
#' @param target_items A vector of names of targeted items
#' @param target_cohorts A vector of names of targeted cohorts
#'
#' @export

dif_analysis <- function(model, target_items, target_cohorts){

    lasso_result <- lasso_restriction(model, target_items, target_cohorts)

    message("Estimation of regression coefficent and classification of DIF:")
    print(lasso_result)
    message("Be aware that LASSO restriction test may be inaccurate when no DIF presents.")
    message("Run MIMIC test?")

    # --- Ask user for decision ---
    user_answer <- readline(
        prompt = "(y/n): "
    )
    user_answer <- tolower(trimws(user_answer))

    # --- Check decision ---
    if (user_answer == "y" || user_answer == "yes") {

        # --- Find the anchor for each domain ---
        domain_df <- tibble::enframe(model$domain_test_map, name="domain", value="lhs") %>%
            tidyr::unnest(cols = c(lhs))

        min_tests_by_domain <- lasso_result %>%
            # adds a 'domain' column to each test
            dplyr::left_join(domain_df, by = "lhs") %>%
            # group the data by domain
            dplyr::group_by(domain) %>%
            # select the row with the minimum 'abs_est' within each group
            dplyr::slice_min(order_by = abs_est, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        anchors = min_tests_by_domain$lhs
        cat('The anchor tests are chosen to be:\n')
        cat(anchors)

        # --- Run MIMIC ---
        mimic_result <- mimic(model, target_items, target_cohorts, anchors)

    } else {
        # --- End the function ---
        message("Stopping function.")
    }
}
