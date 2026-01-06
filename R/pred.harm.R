#' Define a Harmonization Fit Specification
#'
#' This method takes an existing harmonization result and applies the fixed
#' parameters to new data to generate harmonized scores without re-estimation
#'
#' @param harm_fit A `harm_fit` object representing previous harmonization results.
#' @param data A data.frame or a list of data.frame, including new data to be predicted
#'  separated by cohort
#' @param cohorts A string or list of string stating cohorts name, unless `data` is named
#'
#' @return A list or data.frame of scores
#' @export

pred.harm <- function(harm_fit, data, cohorts = NULL){

    # --- 1. Validate input ---

    if(!inherits(harm_fit, "harm_fit")) {
        stop("Input must be a 'harm_fit' object.")
    }

    model <- harm_fit$model
    new.cohorts <- setdiff(names(data), model$cohorts)

    if(length(new.cohorts) > 0){
        stop("Cohort(s)", new.cohorts, "are not in 'harm_fit'.")
    }

    # If user passes a single data.frame
    if (is.data.frame(data)) {
        if(!is.null(cohorts)){
            data <- list(cohorts = data)
        } else{
            data <- list(UNNAMED1 = data)
            warning("The 'data' provided is unnamed. ",
                    "Default cohort names (e.g., 'UNNAMED1') have been assigned.\n",
                    "Consequence: These will be treated as distinct new cohorts. ",
                    "Data matching will rely solely on column names in the data frames.")
        }

    }

    if (!is.list(data)) {
        stop("'data' must be a data.frame or a list of data.frames.")
    }

    # Handle unnamed lists
    if (is.null(names(data))) {
        names(data) <- cohorts
        if(!is.null(cohorts)){
            names(data) <- cohorts
        } else{
            names(data) <- paste0("UNNAMED", seq_along(data))
            warning("The 'data' provided is unnamed. ",
                    "Default cohort names (e.g., 'UNNAMED1') have been assigned.\n",
                    "Consequence: These will be treated as distinct new cohorts. ",
                    "Data matching will rely solely on column names in the data frames.")
        }

    }

    # --- 2. Exam coverage of items ---

    err_messages <- c()

    # Check coverage for each known cohort/dataset provided
    lapply(cohorts, function(cohort_name) {
        df <- data[[cohort_name]]
        correct_items <- model$cohort_test_map[[cohort_name]]

        if (!all.equal(colnames(df), correct_items)) {
            missed_items <- setdiff(correct_items, colnames(df))
            extra_items <- setdiff(colnames(df), correct_items)
            err_message <- c(err_message,
                             paste("Items of cohort",
                             cohort_name,
                             "do not match columns of inut data.frame.\n",
                             "Missed items:", missed_items, "\n",
                             "Extra items:", extra_items))
        }
    })

    if(length(err_messages) > 0){
        stop(paste(err_messages, collapse = '\n'))
    }

    # --- 3. Prediction ---

    scores <- lapply(cohorts, function(cohort_name) {
        df <- data[[cohort_name]]
        fs <- lavaan::lavPredict(harm_fit$fit_results[[cohort_name]],
                                 newdata = df)
        result <- as.data.frame(fs)
        return(result)
    })

    names(scores) <- cohorts

    return(scores)
}
