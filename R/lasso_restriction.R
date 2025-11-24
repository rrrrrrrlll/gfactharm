#' Run LASSO Regularization Test
#'
#' This function run LASSO regularization test on every item of a linear model,
#' predict uniform DIF based on log-scale greatest gap method, and print a plot
#' of regression coefficients and prediction of DIF.
#'
#' Notice: non-uniform DIF causing poor fit
#'
#' @param model A `harm_model` object representing model structure
#' @param target_items A vector of names of targeted items
#' @param target_cohorts A vector of names of targeted cohorts
#'
#' @return A table summarize prediction of DIF on targeted items
#' @export


lasso_restriction <- function(model, target_items, target_cohorts) {

    cat("\n--- Running LASSO Restriction Test ---\n")

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
                               fix_lv_means     = FALSE,
                               fix_lv_variances = TRUE,
                               group_reg        = target_items)

    # Define the labels for the DIF parameters
    dif_labels <- paste0("dif", 1:length(target_items))

    # Print the model being fitted for user transparency
    cat("--- Fitting the following MIMIC model ---\n")
    cat(mimic_syntax)
    cat("\n-----------------------------------------\n\n")


    # 2. Run regularized SEM (LASSO)
    cat("Running regularized SEM with LASSO penalty. This may take a moment...\n")
    initial_fit <- lavaan::sem(mimic_syntax, dat_tmp, check.post = FALSE)
    cv_fit <- regsem::regsem(
        model = initial_fit,
        type = "lasso", # Specify the LASSO penalty
        pars_pen = dif_labels,
        gradFun = "ram"
    )
    cat("Analysis complete.\n\n")

    # 3. Interpret the results
    # We look at the summary, focusing on the estimates for the penalized parameters.
    # Parameters shrunk to zero are considered DIF-free (anchors).
    # Parameters with non-zero estimates are flagged as having DIF.
    # We use log-scale Largest Gap Method to decide DIF
    cat("--- LASSO DIF Detection Results ---\n")
    estimates <- t(summary(cv_fit)$estimates)
    results <- data.frame(
        label = rownames(estimates),
        est = estimates[, 1],
        row.names = NULL
    )

    dif_pattern <- paste0(group_var, " -> ", target_items)
    dif_results <- results[results$label %in% dif_pattern, ]

    # Create the 'lhs' column (item name) for plotting and output
    dif_results$lhs <- sub(paste0(group_var, " -> "), "", dif_results$label)

    # Get absolute values of coefficients and sort them
    dif_results$abs_est <- abs(dif_results$est)
    dif_results <- dif_results[order(dif_results$abs_est), ]

    # Calculate the log-scale gaps between consecutive sorted coefficients
    epsilon <- 1e-2 # Add a small constant to avoid issues with log(0)
    gaps <- diff(log(dif_results$abs_est + epsilon))

    # Find the largest gap.
    if (length(gaps) > 0) {
        cutoff_index <- which.max(gaps)
        threshold <- dif_results$abs_est[cutoff_index]
    } else {
        threshold <- 0.01 # Default for single-item case
    }

    # Classify items
    dif_results$Classification <- ifelse(dif_results$abs_est > threshold, "DIF", "No DIF")
    print(dif_results)

    # Create a nice summary plot
    p <- ggplot2::ggplot(
             dif_results,
             ggplot2::aes(x = reorder(lhs, abs_est), y = abs_est, color = Classification)
         ) +
         ggplot2::geom_point(size = 4) +
         ggplot2::geom_hline(
             yintercept = threshold + 0.01,
             linetype = "dashed",
             color = "blue"
         ) +
         ggplot2::coord_flip() +
         ggplot2::scale_color_manual(
             values = c("DIF" = "darkgreen", "NO DIF" = "darkred")
         ) +
         ggplot2::labs(
             title = "Automated DIF Classification w/ Largest Gap Method",
             subtitle = paste0("Cutoff threshold identified at: ", round(threshold, 3)),
             x = "Item",
             y = "Absolute Magnitude of DIF Coefficient"
         ) +
         ggplot2::theme_minimal(base_size = 14)

    print(p)

    return(dif_results[, c("lhs", "est", "abs_est", "Classification")])
}
