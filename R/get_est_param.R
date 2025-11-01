#' Extracts loadings, intercepts means, and residual variances for a
#' given list of items from a lavaan parameterEstimates data frame.
#'
#' @param estimates_df A data.frame returned from `lavaan::parameterEstimates()`.
#' @param item_names A character vector of the observed variable names (items)
#'                   you want to extract parameters for.
#'
#' @return A data.frame with n rows (one for each item) and 3 columns:
#'         "loading", "mean", and "variance".

get_est_param <- function(estimates_df, item_names) {

    # Create a matrix where each column corresponds to an item
    # and rows are the parameters (loading, mean, variance).
    results_matrix <- sapply(item_names, function(item) {

        # 1. Extract Loading
        loading <- estimates_df[estimates_df$op == "=~" &
                                    estimates_df$rhs == item, "est"]

        # Handle cases where an item might have multiple loadings
        if (length(loading) > 1) {
            stop(paste("Item", item, "has multiple loadings."))
        }

        # If no loading is found, report error
        if (length(loading) == 0) {
            stop(paste("Item", item, "is not an indicator in the model."))
        }

        # 2. Extract Mean (Intercept)
        mean_val <- estimates_df[estimates_df$op == "~1" & estimates_df$lhs == item, "est"]
        if (length(mean_val) == 0) {
            stop(paste("Item", item, "has no estimated intercept."))
        }

        # 3. Extract Residual Variance
        variance <- estimates_df[estimates_df$op == "~~" &
                                     estimates_df$lhs == item &
                                     estimates_df$rhs == item, "est"]
        if (length(variance) == 0) {
            stop(paste("Item", item, "has no estimated variance."))
        }

        # Return a named vector for this item
        return(c(item = item, loading = loading, mean = mean_val, variance = variance))
    })

    # Transpose the matrix so that items are rows and parameters are columns
    # Then convert it to a data.frame
    results_df <- as.data.frame(t(results_matrix))

    # Ensure column names are set correctly
    colnames(results_df) <- c("item", "loading", "mean", "var")

    return(results_df)
}
