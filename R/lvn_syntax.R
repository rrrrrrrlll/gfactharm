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

    # Extract information from `model`
    domains = model$domains
    d_t_map = model$domain_test_map

    # Initialize character vector to hold parts of the syntax
    syntax_parts <- c()

    # Keep track of which domains are actually used
    relevant_domains <- c()


    # --- Part 1: Latent Variable Definitions (=~) ---
    lv_defs <- c()

    for (domain in domains) {

        # Find which indicators for THIS LV are in the selected list
        relevant_indicators <- intersect(d_t_map[[domain]], selected_items)

        # If at least one indicator is present, create the syntax line
        if (length(relevant_indicators) > 0) {
            # Add this domain to our list of domains to model
            relevant_domains <- c(relevant_domains, domain)

            # Vector to hold individual item strings (e.g., "item" or "0.8*item")
            indicator_strings <- c()

            for (item in relevant_indicators) {
                if (!is.null(loadings) && (item %in% names(loadings))) {
                    # This item's loading is fixed
                    item_str <- paste0(loadings[item], "*", item)
                } else {
                    # This item's loading is free (or marker)
                    item_str <- paste0("NA*", item)
                }
                indicator_strings <- c(indicator_strings, item_str)
            }

            # Combine all item strings to be the right-hand-side of a line
            rhs <- paste(indicator_strings, collapse = " + ")

            # Create the syntax line for this LV
            lv_defs <- c(lv_defs, paste(domain, "=~", rhs))
        }
    }

    if (length(lv_defs) > 0) {
        syntax_parts <- c(syntax_parts,
                          "# --- Latent variable definitions ---",
                          lv_defs)
    }
    else {
        stop('The model contains 0 latent variable')
    }


    # --- Part 2: Latent Variable Intercepts (~~) ---

    if(fix_lv_means){
        syntax_parts <- c(syntax_parts,
                          "\n# --- Fixation of intercepts ---")
        for(domain in relevant_domains){
            syntax_parts <- c(syntax_parts,
                              paste0(domain, " ~ ", fixed_intercept, "*1"))
        }
    }

    # --- Part 3: Latent Variable Variance (~~) ---

    if(fix_lv_variances){
        syntax_parts <- c(syntax_parts,
                          "\n# --- Fixation of variances ---")
        for(domain in relevant_domains){
            syntax_parts <- c(syntax_parts,
                              paste0(domain, " ~~ ", fixed_variance, "*", domain))
        }
    }


    # --- Part 4: Group variance

    if(!is.null(group_reg)){
        group_var <- model$group_var

        # the main effect (regressions of latent variables)
        main_effect <- paste0(paste0(relevant_domains, collapse = " + "),
                             " ~ ",
                             group_var)

        # the direct effect (regressions of target items)
        dif_labels <- paste0("dif", 1:length(group_reg))
        direct_effect <- paste0(group_reg,
                               " ~ ",
                               dif_labels,
                               "*",
                               group_var)

        syntax_parts <- c(syntax_parts,
                          "\n# --- Group Variable Regression ---",
                          main_effect,
                          direct_effect)
    }


    # --- Final Step: Combine and Print ---
    final_syntax <- paste(syntax_parts, collapse = "\n")

    # Print the syntax to the console
    # cat(final_syntax)

    # Return the syntax string invisibly (so it can be assigned to a variable)
    return(invisible(final_syntax))
}
