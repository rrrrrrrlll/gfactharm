#' Generate `lavaan` Syntax of Second Order General Factor Linear Model
#'
#' Given full model information, generate model syntax of a 2st order linear model
#' include only selected items.
#'
#' @param model A `harm_model` object representing model structure.
#' @param selected_items A character vector of names of selected items.
#'
#' @param ob_loadings A named numeric vector. Names are selected item names,
#' values are the loadings to fix. Items not in this vector will be freely estimated.
#'  If `NULL` (default), all loadings are freely estimated.
#' @param ob_mean A named numeric vector. Names are selected item names,
#' values are the intercepts to fix. Items not in this vector will be freely estimated.
#'  If `NULL` (default), all intercepts are freely estimated.
#' @param ob_var A named numeric vector. Names are selected item names,
#' values are the variances to fix. Items not in this vector will be freely estimated.
#'  If `NULL` (default), all variances are freely estimated.
#'
#' @param lv_loadings A named numeric vector. Names are selected domain factor names,
#' values are the loadings to fix. Domain factors not in this vector will be
#' freely estimated.
#'  If `NULL` (default), all loadings are freely estimated.
#'
#' @param std_g Logical.
#'  If `TRUE`, general factor intercept and variance, and esidual variance of latent
#'  variables will be standardized.
#'  If `FALSE` (default), these parameters are freely estimated.
#' @param gf A string representing the general factor's name. Default to `G`.
#'
#' @return A character string of the formatted lavaan model syntax.


lvn_syntax_g <- function(model,
                       selected_items,
                       lv_loadings = NULL,
                       ob_loadings = NULL,
                       ob_mean     = NULL,
                       ob_var      = NULL,
                       std_g       = FALSE,
                       gf          = 'G') {

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
                if (!is.null(ob_loadings) && (item %in% names(ob_loadings))) {
                    item_str <- paste0(ob_loadings[item], "*", item)
                } else {
                    item_str <- paste0("NA*", item)
                }
                indicator_strings <- c(indicator_strings, item_str)
            }

            rhs <- paste(indicator_strings, collapse = " + ")
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


    # --- Part 2: General Variable Definition (=~) ---
    domain_strings <- c()

    for(domain in relevant_domains){
        if (!is.null(lv_loadings) && (domain %in% names(lv_loadings))) {
            domain_str <- paste0(lv_loadings[domain], "*", domain)
        } else {
            domain_str <- paste0("NA*", domain)
        }
        domain_strings <- c(domain_strings, domain_str)
    }

    rhs <- paste0(domain_strings, collapse = ' + ')
    syntax_parts <- c(syntax_parts,
                      "# --- General variable definition ---",
                      paste(gf, '=~', rhs))


    # --- Part 3: Item Intercepts (~ 1) ---
    mean_lines <- c()

    if (!is.null(ob_mean)) {
        for (item in selected_items) {
            if (item %in% names(ob_mean)) {
                mean_line <- paste(item, "~", ob_mean[item], "*1")
                mean_lines <- c(mean_lines, mean_line)
            }
        }
    }

    if (length(mean_lines) > 0) {
        syntax_parts <- c(
            syntax_parts,
            "# --- Item Intercepts (Fixed) ---",
            mean_lines
        )
    }


    # --- Part 4: Item Residual Variances (~~) ---
    var_lines <- c()

    if (!is.null(ob_var)) {
        for (item in selected_items) {
            if (item %in% names(ob_var)) {
                var_line <- paste(item, "~~", ob_var[item], "*", item)
                var_lines <- c(var_lines, var_line)
            }
        }
    }

    # Add to main syntax parts if we generated any lines
    if (length(var_lines) > 0) {
        syntax_parts <- c(
            syntax_parts,
            "# --- Item Residual Variances (Fixed) ---",
            var_lines
        )
    }


    # --- Part 5: General Variable Identification ---
    if(std_g){
        # fix mean and variance
        syntax_parts <- c(
            syntax_parts,
            "# --- General Variable Identification ---",
            paste0(gf, ' ~ 0*1'),
            paste0(gf, ' ~~ 1*', gf)
        )

        # label domain factor loadings
        loading_labels <- paste0("g", 1:length(relevant_domains))
        rhs <- paste0(loading_labels, '*', relevant_domains, collapse = ' + ')
        syntax_parts <- c(
            syntax_parts,
            paste(gf, '=~', rhs)
        )

        # label domain factor variances
        var_labels <- paste0('v', 1:length(relevant_domains))
        var_strings <- paste0(relevant_domains, ' ~~ ', var_labels, '*', relevant_domains)
        syntax_parts <- c(
            syntax_parts,
            var_strings
        )

        # set constraints on variances
        constraint_strings <- paste0(var_labels, ' == 1 - ', loading_labels, '^2')
        syntax_parts <- c(
            syntax_parts,
            constraint_strings
        )
    }else{
        syntax_parts <- c(syntax_parts,
                          "# --- General Variable Identification ---",
                          paste(gf, "~ NA*1"),
                          paste(gf, "~~ NA*G"))
    }


    # --- Final Step: Combine and Print ---
    final_syntax <- paste(syntax_parts, collapse = "\n")

    # Print the syntax to the console
    # cat(final_syntax)

    # Return the syntax string invisibly (so it can be assigned to a variable)
    return(invisible(final_syntax))
}
