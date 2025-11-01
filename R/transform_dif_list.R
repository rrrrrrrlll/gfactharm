#' Transforms an item-centric DIF list to a cohort-centric DIF list
#'
#' @param dif_list The input list
#' @return A new list, named by cohort, where each element is a
#'         named vector of `item = group_id`.


transform_dif_list <- function(dif_list) {

    cohort_centric_list <- list()

    # Loop over each item
    for (item_name in names(dif_list)) {

        item_groups <- dif_list[[item_name]]

        # Loop over the DIF groups for that item
        # 'group_id' will be 1, 2, 3...
        for (group_id in seq_along(item_groups)) {

            cohort_group <- item_groups[[group_id]]

            # Loop over each cohort in the group
            for (cohort_name in cohort_group) {

                if (is.null(cohort_centric_list[[cohort_name]])) {
                    cohort_centric_list[[cohort_name]] <- list(
                        item = c(),
                        dif_group = c()
                    )
                }

                # Append the item name and its group ID
                cohort_centric_list[[cohort_name]]$item <- c(
                    cohort_centric_list[[cohort_name]]$item,
                    item_name
                )
                cohort_centric_list[[cohort_name]]$dif_group <- c(
                    cohort_centric_list[[cohort_name]]$dif_group,
                    group_id
                )
            }
        }
    }

    return(cohort_centric_list)
}
