#' Run a Systematic Exploratory Factor Analysis (EFA)
#'
#' This function performs EFA using the 'psych' package. If the number of
#' factors is not provided, it automatically uses Parallel Analysis to
#' determine the optimal number.
#'
#' @param data A data frame or matrix of raw data.
#' @param nfactors Integer. The number of factors to extract. If NULL (default),
#'   Parallel Analysis is run to determine this automatically.
#' @param rotate Character. The rotation method (default: "oblimin").
#' @param fm Character. Factoring method (default: "ml").
#' @param show_plot Logical. If TRUE, shows the Scree/Parallel Analysis plot.
#'
#' @return A list containing:
#' \item{fit}{The raw 'psych::fa' object.}
#' \item{nfactors}{The number of factors used.}
#' \item{loadings}{A clean data frame of factor loadings.}
#' \item{reliability}{Reliability summary (Alpha/Omega) if available.}
#' @export
#' @importFrom psych fa fa.parallel

run_efa <- function(data, nfactors = NULL, rotate = "oblimin", fm = "ml", show_plot = FALSE) {

    # 1. Handle Missing Data (Simple listwise deletion for EFA)
    # You might want to add more complex handling or warnings here
    data_clean <- na.omit(data)

    # 2. Determine Number of Factors (if not provided)
    if (is.null(nfactors)) {
        message("No 'nfactors' specified. Running Parallel Analysis to determine optimal number...")

        # Run Parallel Analysis
        pa_results <- psych::fa.parallel(
            data_clean,
            fm = fm,
            fa = "fa",
            plot = show_plot,
            n.iter = 50,
            show.legend = FALSE
        )

        # Extract the suggested number of factors
        nfactors <- pa_results$nfact
        message(paste("Parallel Analysis suggests:", nfactors, "factors."))

        if (nfactors == 0) {
            warning("Parallel analysis suggests 0 factors. Defaulting to 1.")
            nfactors <- 1
        }
    }

    # 3. Run the EFA
    fit <- psych::fa(
        r = data_clean,
        nfactors = nfactors,
        rotate = rotate,
        fm = fm
    )

    # 4. Clean up Loadings for easier reading
    # Unclass converts the specialized matrix to a standard one
    loadings_df <- as.data.frame(unclass(fit$loadings))

    # Add variable names as a column
    loadings_df$item <- rownames(loadings_df)

    # Reorder columns so 'item' is first
    loadings_df <- loadings_df[, c("item", setdiff(names(loadings_df), "item"))]

    # 5. Construct Output List
    result <- list(
        fit = fit,
        nfactors = nfactors,
        loadings = loadings_df,
        settings = list(rotate = rotate, fm = fm)
    )

    return(result)
}
