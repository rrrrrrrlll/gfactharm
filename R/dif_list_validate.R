#' Validate All Harmonization Inputs
#'
#' Runs a series of validation checks on the model and DIF lists
#' before starting harmonization.
#'
#' @param model An object of class 'harm_model'.
#' @param unidif A list describing uniform DIF.
#' @param nonunidif A list describing non-uniform DIF.

dif_list_validate <- function(model, unidif = NULL, nonunidif = NULL) {

    # --- 1. Basic Model Integrity ---
    if (!inherits(model, "harm_model")) {
        stop("'model' must be an object of class 'harm_model'.")
    }

    # Check for essential components
    required_elements <- c("data", "cohorts", "domains", "domain_test_map")
    missing_elements <- setdiff(required_elements, names(model))
    if (length(missing_elements) > 0) {
        stop(paste("'model' (harm_model) is missing required elements:",
                   paste(missing_elements, collapse = ", ")))
    }

    if (length(model$cohorts) < 2) {
        stop("'model$cohorts' must contain at least two cohorts.")
    }

    # Check data consistency
    if (!setequal(model$cohorts, names(model$data))) {
        stop("Names in 'model$cohorts' and names in 'model$data' do not match.")
    }

    if (!is.list(unidif) && !is.null(unidif)) {
        stop("'unidif' must be a list.")
    }
    if (!is.list(nonunidif) && !is.null(nonunidif)) {
        stop("'nonunidif' must be a list.")
    }

    # --- 2. Existence of DIF Items and Cohorts

    error_msg <- c()

    items   <- model$items
    cohorts <- model$cohorts
    domains <- model$domains

    if(!is.null(unidif)){
        # existence of items
        missing_items <- setdiff(names(unidif), items)
        if(length(missing_items) > 0){
            error_msg <- c(error_msg,
                           paste("'model' missing items",
                                 missing_items,
                                 "from 'unidif'")
                           )
        }
        #existence of cohorts
        missing_cohorts <- setdiff(unlist(unidif), cohorts)
        if(length(missing_cohorts) > 0){
            error_msg <- c(error_msg,
                           paste("'model' missing cohorts",
                                 missing_cohorts,
                                 "from 'unidif'")
                           )
        }
    }

    if(!is.null(nonunidif)){
        # existence of items
        missing_items <- setdiff(names(nonunidif), items)
        if(length(missing_items) > 0){
            error_msg <- c(error_msg,
                           paste("'model' missing items",
                                 missing_items,
                                 "from 'nonunidif'")
            )
        }
        # existence of cohort
        missing_cohorts <- setdiff(unlist(nonunidif), cohorts)
        if(length(missing_cohorts) > 0){
            error_msg <- c(error_msg,
                           paste("'model' missing cohorts",
                                 missing_cohorts,
                                 "from 'nonunidif'")
            )
        }
    }


    # --- 3. Overlap of DIF Items ---

    # overlap within the list
    for(item in names(unidif)){
        cohorts_of_item <- unlist(unidif[[item]])
        if(length(unique(cohorts_of_item)) < length(cohorts_of_item)){
            error_msg <- c(error_msg,
                           paste("Some cohort(s) of item", item,
                                 "appears in multiple DIF groups in 'unidif'"))
        }
    }
    for(item in names(nonunidif)){
        cohorts_of_item <- unlist(nonunidif[[item]])
        if(length(unique(cohorts_of_item)) < length(cohorts_of_item)){
            error_msg <- c(error_msg,
                           paste("Some cohort(s) of item", item,
                                 "appears in multiple DIF groups in 'nonunidif'"))
        }
    }

    #overlap across the lists
    if(!is.null(unidif) && !is.null(nonunidif)){
        overlap_items <- intersect(names(unidif), names(nonunidif))
        for(item in overlap_items){
            overlap_cohorts <- intersect(unlist(unidif[[item]]), unlist(nonunidif[[item]]))
            if(length(overlap_cohorts) > 0){
                error_msg <- c(error_msg,
                               paste("Item", item, "in cohort(s)", overlap_cohorts,
                                     "ovelaps in uniform and nonuniform DIF lists"))
            }
        }
    }


    # --- 4. Properties of Reference Cohort

    reference_cohort <- model$cohorts[1]
    reference_items <- model$cohort_test_map[[reference_cohort]]

    # include all domains
    for(domain in domains){
        intersect <- intersect(model$domain_test_map[[domain]], reference_items)
        if(length(intersect) == 0){
            error_msg <- c(error_msg,
                           paste("The reference cohort", reference_cohort,
                                 "must contain tests in ", domain))
        }
    }

    # no DIF items
    if(reference_cohort %in% unlist(unidif)){
        error_msg <- c(error_msg,
                       paste("The reference cohort", reference_cohort,
                             "should not have uniform DIF items"))
    }
    if(reference_cohort %in% unlist(nonunidif)){
        error_msg <- c(error_msg,
                       paste("The reference cohort", reference_cohort,
                             "should not have nonuniform DIF items"))
    }

    if(length(error_msg) > 0){
        stop(paste0(
            "Validation failed due to following error(s):\n",
            paste(error_msg, collapse = "\n")
        ))
    }

    message("All harmonization inputs are valid.")
}
