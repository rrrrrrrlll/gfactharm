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

    message("Estimation of regression coefficent and classification of DIF.")
    print(prelim_results)
    message("Be aware that LASSO restriction method may not be accurate when no DIF presents.")
    message("Run MIMIC test?")

    # --- Ask user for decision ---
    user_answer <- readline(
        prompt = "(y/n): "
    )
    user_answer <- tolower(trimws(user_answer))

    # --- Check decision ---
    if (user_answer == "y" || user_answer == "yes") {

        # --- Run MIMIC ---
        mimic_result <- "later :)"

    } else {
        # --- End the function ---
        message("Stopping function.")
    }
}
