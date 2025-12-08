#' Harmonize the Data Given Full Model Strucuture
#'
#' This function harmonize the data across cohorts if given `harm_model`, in order
#' of `harm_model$cohorts`. DIF items can be specified in advance. A named list of
#' fit results and a list of fixed parameters will be return
#'
#' @param model A `harm_model` object representing model structure.
#' @param unidif A named list specifying the cohorts that each items is believed
#' to have uniform DIF. Default to be `NULL`.
#' @param nonunidif A named list specifying the cohorts that each items is believed
#' to have nonuniform DIF. Default to be `NULL`.
#'
#' @return A named of of fit results and the vector of all parameter values.
#' @export
#'

harmonization <- function(model,
                          unidif = NULL,
                          nonunidif = NULL){


    # --- 0. Validate 'model' ---
    dif_list_validate(model, unidif, nonunidif)

    # --- 1. Preparation ---
    data_list <- model$data
    cohorts <- model$cohorts
    domains <- model$domains
    c_t_map <- model$cohort_test_map
    fit_results <- list()

    # record fixed parameters after harmonization
    default_param <- data.frame(
        item = c(),
        loading = c(),
        mean = c(),
        var = c()
    )
    unidif_param <- data.frame(matrix(ncol = 4,
                                      nrow = 0,
                                      dimnames = list(NULL, c("item", "dif_group", "mean", "var"))),
                               stringsAsFactors = FALSE) %>%
        dplyr::mutate(item = as.character(item),
                      dif_group = as.integer(dif_group),
                      mean = as.numeric(mean),
                      var = as.numeric(var))

    nonunidif_param <- data.frame(matrix(ncol = 5,
                                         nrow = 0,
                                         dimnames = list(NULL, c("item", "dif_group", "loading", "mean", "var"))),
                                  stringsAsFactors = FALSE) %>%
        dplyr::mutate(item = as.character(item),
                      dif_group = as.integer(dif_group),
                      loading = as.numeric(loading),
                      mean = as.numeric(mean),
                      var = as.numeric(var))

    # transform the DIF lists to cohort-centric lists
    unidif_cohorts <- transform_dif_list(unidif)
    nonunidif_cohorts <- transform_dif_list(nonunidif)


    # --- 2. Fit Baseline Cohort ---

    baseline_cohort <- cohorts[1]
    baseline_items <- c_t_map[[baseline_cohort]]
    syntax <- lvn_syntax_g(model,
                           selected_items = model$cohort_test_map[[baseline_cohort]],
                           std_g = TRUE)
    fit_results[[baseline_cohort]] <- lavaan::sem(model = syntax,
                                                  data = data_list[[baseline_cohort]])
    baseline_est_param <- lavaan::parameterEstimates(fit_results[[baseline_cohort]])

    # get loadings of latent(domain) variables
    lv_param <- get_est_param(baseline_est_param, domains)
    lv_loadings <- lv_param$loading
    names(lv_loadings) <- lv_param$item

    # get parameters of items in baseline cohort
    ob_param <- get_est_param(baseline_est_param, baseline_items)
    default_param <- rbind(default_param, ob_param)

    cat(paste('1 in', length(cohorts), 'cohorts has been harmonized.\n'))


    # --- 3. Harmonize Other Cohorts ---

    # Define an empty df structure for a 0-row unidif/nonunidif item set
    empty_dif_df <- data.frame(item = character(),
                               dif_group = integer(),
                               stringsAsFactors = FALSE)

    for(index in 2:length(cohorts)) {
        cohort <- cohorts[index]
        cat(paste("\n--- Harmonizing Cohort:", cohort,
                  "(", index-1, "of", length(cohorts)-1, ") ---\n"))

        # 1. Separate items by DIF types
        all_items <- c_t_map[[cohort]]

        # Get DIF items, or an empty DF if NULL
        unidif_items <- if (!is.null(unidif_cohorts[[cohort]])) {
            unidif_cohorts[[cohort]]
        } else {
            empty_dif_df
        }
        nonunidif_items <- if (!is.null(nonunidif_cohorts[[cohort]])) {
            nonunidif_cohorts[[cohort]]
        } else {
            empty_dif_df
        }

        # Get item names
        unidif_item_names <- unidif_items$item
        nonunidif_item_names <- nonunidif_items$item
        nodif_items <- setdiff(all_items, c(unidif_item_names, nonunidif_item_names))

        cat(paste("  Items (No DIF):", length(nodif_items), "\n"))
        cat(paste("  Items (Uni DIF):", length(unidif_item_names), "\n"))
        cat(paste("  Items (NonUni DIF):", length(nonunidif_item_names), "\n"))

        # 2. For each part, get fixed and unfixed items

        # No DIF: Compare against 'default_param' item list
        nodif_fix <- intersect(nodif_items, default_param$item)
        nodif_free <- setdiff(nodif_items, nodif_fix)

        # Uniform DIF: Compare against 'unidif_param' (item + group_id)
        unidif_fix <- dplyr::semi_join(unidif_items, unidif_param, by = c("item", "dif_group"))
        unidif_free <- dplyr::anti_join(unidif_items, unidif_param, by = c("item", "dif_group"))

        # Nonuniform DIF: Compare against 'nonunidif_param' (item + group_id)
        nonunidif_fix <- dplyr::semi_join(nonunidif_items, nonunidif_param, by = c("item", "dif_group"))
        nonunidif_free <- dplyr::anti_join(nonunidif_items, nonunidif_param, by = c("item", "dif_group"))

        cat(paste("  [No DIF] Fixed:", length(nodif_fix), "| Free:", length(nodif_free), "\n"))
        cat(paste("  [Uni DIF] Fixed:", nrow(unidif_fix), "| Free:", nrow(unidif_free), "\n"))
        cat(paste("  [NonUni DIF] Fixed:", nrow(nonunidif_fix), "| Free:", nrow(nonunidif_free), "\n"))

        # 3. Get values of fixed parameters
        ob_loadings <- c()
        ob_mean <- c()
        ob_var <- c()

        # Get 'No DIF' fixed params
        if (length(nodif_fix) > 0) {
            nodif_param_vals <- dplyr::filter(default_param, item %in% nodif_fix)
            ob_loadings <- c(ob_loadings,
                             setNames(nodif_param_vals$loading, nodif_param_vals$item))
            ob_mean <- c(ob_mean,
                         setNames(nodif_param_vals$mean, nodif_param_vals$item))
            ob_var <- c(ob_var,
                        setNames(nodif_param_vals$var, nodif_param_vals$item))
        }

        # Get 'Uniform DIF' fixed params
        if (nrow(unidif_fix) > 0) {
            nodif_param_vals <- dplyr::filter(default_param, item %in% unidif_fix$item)
            unidif_param_vals <- dplyr::semi_join(unidif_param, unidif_fix,
                                                  by = c("item", "dif_group"))
            ob_loadings <- c(ob_loadings,
                             setNames(nodif_param_vals$loading, unidif_param_vals$item))
            ob_mean <- c(ob_mean,
                         setNames(unidif_param_vals$mean, unidif_param_vals$item))
            ob_var <- c(ob_var,
                        setNames(unidif_param_vals$var, unidif_param_vals$item))
        }

        # Get 'Nonuniform DIF' fixed params
        if (nrow(nonunidif_fix) > 0) {
            nonunidif_param_vals <- dplyr::semi_join(nonunidif_param, nonunidif_fix,
                                                     by = c("item", "dif_group"))
            ob_loadings <- c(ob_loadings,
                             setNames(nonunidif_param_vals$loading, nonunidif_param_vals$item))
            ob_mean <- c(ob_mean,
                         setNames(nonunidif_param_vals$mean, nonunidif_param_vals$item))
            ob_var <- c(ob_var,
                        setNames(nonunidif_param_vals$var, nonunidif_param_vals$item))
        }

        # 4. Define syntax and fit

        # Check if there is anything to estimate
        all_free_items <- c(nodif_free, unidif_free$item, nonunidif_free$item)
        if (length(all_free_items) == 0) {
            cat("  All items are fixed. No new parameters to estimate. Skipping fit.\n")
            fit_results[[cohort]] <- NULL # Or a "fixed" model
            next # Skip to the next cohort
        }

        syntax <- lvn_syntax_g(model = model,
                               selected_items = all_items,
                               ob_loadings = ob_loadings,
                               ob_mean = ob_mean,
                               ob_var = ob_var,
                               lv_loadings = lv_loadings)

        cat("\n--- Fitting the following model ---\n\n")
        cat(syntax)
        cat("\n-----------------------------------\n\n")
        fit <- lavaan::sem(model = syntax, data = data_list[[cohort]])
        fit_results[[cohort]] <- fit
        raw_estimates <- lavaan::parameterEstimates(fit)


        # 5. Get and store the newly fixed parameter values

        # No DIF
        if (length(nodif_free) > 0) {
            new_default_param <- get_est_param(raw_estimates, nodif_free)
            default_param <- rbind(default_param, new_default_param)
        }

        # Uniform DIF
        if (nrow(unidif_free) > 0) {
            new_uni_param <- get_est_param(raw_estimates, unidif_free$item)
            new_uni_param_grouped <- dplyr::left_join(unidif_free, new_uni_param, by = 'item')
            new_uni_param_grouped$loading <- NULL
            unidif_param <- rbind(unidif_param, new_uni_param_grouped)
        }

        # Nonuniform DIF
        if (nrow(nonunidif_free) > 0) {
            new_nonuni_param <- get_est_param(raw_estimates, nonunidif_free$item)
            new_nonuni_param_grouped <- dplyr::left_join(nonunidif_free, new_nonuni_param, by = 'item')
            nonunidif_param <- rbind(nonunidif_param, new_nonuni_param_grouped)
        }

        cat(paste("  Cohort", cohort, "harmonized.\n"))
    }

    # --- 6. Get Factor Scores ---

    scores_list <- lapply(names(fit_results), function(grp_name) {
        fs <- lavaan::lavPredict(fit_results[[grp_name]])
        df <- as.data.frame(fs)
        df$cohort <- grp_name
        return(df)
    })

    # Combine into one df
    factor_scores <- dplyr::bind_rows(scores_list)

    # --- 7. Return all results ---

    message('Harmonizarion Completed.')
    return(list(
        fit_results = fit_results,
        factor_scores = factor_scores,
        default_param = default_param,
        unidif_param = unidif_param,
        nonunidif_param = nonunidif_param
    ))
}
