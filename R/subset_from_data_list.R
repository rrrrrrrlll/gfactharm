#' Get and Validate a Data Subset for cross-cohort Analysis
#'
#' This function subsets a dataframe.
#' It validates that the specified groups and item columns exist before returning
#' a filtered dataframe.
#'
#' @param model A `harm_model` object representing model structure
#' @param target_items A character vector of the item column names to select.
#' @param target_groups A character vector of the cohort names to select.
#'   that identifies the groups. Defaults to "cohort".
#'
#' @return A dataframe containing only the rows for 'target_groups' and
#'   the columns for 'group_col' and 'items'.


subset_from_data_list <- function(model, target_items, target_groups) {

    # --- 0. Extract information from model ---
    group_col <- model$group_var
    data_list <- model$data
    c_t_map <- model$cohort_test_map

    # --- 1. Validate Target Groups ---
    # First, check that the groups themselves exist in the list
    missing_groups <- setdiff(target_groups, model$cohorts)
    if (length(missing_groups) > 0) {
        stop(paste0(
            "The following group(s) were not found as `model$cohorts`: ",
            paste(missing_groups, collapse = ", "),
            ". Available groups are: ", paste(available_groups, collapse = ", ")
        ))
    }

    # --- 2. Build List of Processed Dataframes (with new validation) ---
    processed_list <- list()
    error_messages <- c()

    for (group_name in target_groups) {

        # --- New Strict Validation ---
        # Check if all required items are present in this group's dataframe
        missing_items_in_group <- setdiff(target_items, c_t_map$group_name)

        if (length(missing_items_in_group) > 0) {
            # Store error message instead of stopping immediately
            # This allows us to report *all* errors at once
            msg <- paste0(
                "  - Group '", group_name,
                "' is missing required item(s): ",
                paste(missing_items_in_group, collapse = ", ")
            )
            error_messages <- c(error_messages, msg)
            next # Skip to the next group
        }


        # --- End New Validation ---

        # Get the raw dataframe for this group
        group_data_raw <- data_list[[group_name]]

        # If subset to only the items we want
        group_data_processed <- group_data_raw[, target_items, drop = FALSE]

        # Add the group identifier column
        group_data_processed[[group_col]] <- group_name

        # Add to our list
        processed_list[[group_name]] <- group_data_processed
    }

    # --- 3. Check for and Report Errors ---
    if (length(error_messages) > 0) {
        stop(paste0(
            "Validation failed. Required items were missing from target groups:\n",
            paste(error_messages, collapse = "\n")
        ))
    }

    # --- 4. Combine into a Single Dataframe ---
    # This code only runs if no errors were found
    final_data <- do.call(rbind, processed_list)

    # Re-order columns to be tidy: group_col first, then items
    final_data <- final_data[, c(group_col, items)]

    # Clean up row names
    rownames(final_data) <- NULL

    # Convert group column to factor
    #final_data[[group_col]] <- as.factor(final_data[[group_col]])

    cat(paste0(
        "Successfully built dataframe with ", nrow(final_data),
        " observations for ", length(target_groups), " group(s) and ",
        length(items), " item(s).\n"
    ))

    return(final_data)
}
