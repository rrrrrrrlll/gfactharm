#' Fit a `harm_model` into First Order Linear Models
#'
#' Given full model information and selected items and groups, for each group,
#' fit the data with a individual first order linear model.
#' Loadings are set free. Standardizing means and variances by default.
#'
#' @param model A `harm_model` object representing model structure.
#' @param selected_items A name or character vector of names of selected item(s).
#' @param selected_cohorts A name or A character vector of names of selected cohort(s).
#' @param std Logical. If true standardize means and variances. Default to `TRUE`.
#'
#' @return a fit result, or a named list of fit results with `lavaan::sem`
#' @export

fit1st <- function(model, selected_items, selected_cohorts, std=TRUE){

    if(!is.character(selected_cohorts)){
        stop("'Selected_cohorts' must be a character or a charactor vector")
    }

    c_t_map <- model$cohort_test_map
    syntax <- lvn_syntax(
        model,
        selected_items,
        fix_lv_means = std,
        fix_lv_variances = std
    )

    # --- In case of selected_cohort being a character
    if(length(selected_cohorts) == 1){
        missing_items <- setdiff(selected_items, c_t_map[[selected_cohorts]])
        if(length(missing_items) > 0){
            stop(paste0(
                "  - Cohort '", selected_cohorts,
                "' is missing required item(s): ",
                paste(missing_items, collapse = ", ")
            ))
        }

        data <- df_from_data_list(model, selected_items, selected_cohorts)
        return(lavaan::sem(syntax, data = data))
    }

    # --- Build List of fit outcome
    fit_list <- list()
    error_messages <- c()

    for(cohort in selected_cohorts){
        missing_items <- setdiff(selected_items, c_t_map[[cohort]])
        if(length(missing_items) > 0){
            error_messages <- c(error_messages,
                                paste0(
                                   "  - Cohort '", cohort,
                                   "' is missing required item(s): ",
                                   paste(missing_items, collapse = ", ")
                               ))
            next
        }

        data <- df_from_data_list(model, selected_items, cohort)
        fit_list[[cohort]] <- lavaan::sem(syntax, data = data)
    }

    if(length(error_messages) > 0){
        stop(paste0(
            "Validation failed. Required items were missing from target groups:\n",
            paste(error_messages, collapse = "\n")
        ))
    }

    return(fit_list)
}
