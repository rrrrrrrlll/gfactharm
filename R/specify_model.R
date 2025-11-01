#' Define a Harmonization Model Specification
#'
#' This function creates a 'harm_model' object that stores the model structure,
#' data, and settings for use in later functions.
#'
#' @param cohorts A vector of names of all cohorts.
#' @param cohort_domain_test_map A map showing which tests belongs to each cohort's
#'      each domain.
#' @param covariate_lst A list showing which ethnics info is presented in each cohort
#' @param preferred_domain_order A vector show preferred order of domains
#' @param data A named list, store the raw data by cohort.
#' @param group_var A string, the name of the grouping variable. Default to 'cohort'.
#'
#' @return An object of class 'harm_model'.
#' @export

specify_model <- function(cohorts,
                          cohort_domain_test_map,
                          covariate_lst,
                          preferred_domain_order = c(),
                          data,
                          group_var = 'cohort') {

    ## cohort_domain_map: domains present per cohort from domain_tests_map
    ## Preserve a preferred order where applicable
    cohort_domain_map <- lapply(cohort_domain_test_map, function(dm) {
        doms <- names(dm)
        doms[order(match(doms, preferred_domain_order,
                         nomatch = length(preferred_domain_order) + 1))]
    })

    ## Derive domain_test_map by union across cohorts for each domain
    domains <- unique(unlist(lapply(cohort_domain_test_map, names)))
    domains <- domains[order(match(domains, preferred_domain_order,
                                   nomatch = length(preferred_domain_order) + 1))]

    domain_test_map <- setNames(lapply(domains, function(dom) {
        unique(unlist(lapply(cohort_domain_test_map, function(dm) dm[[dom]])))
    }), domains)

    ## cohort_test_map by concatenating tests in preferred domain order
    cohort_test_map <- lapply(cohort_domain_test_map, function(dm) {
        doms <- names(dm)
        doms <- doms[order(match(doms, preferred_domain_order,
                                 nomatch = length(preferred_domain_order) + 1))]
        unique(unlist(dm[doms], use.names = FALSE))
    })

    items <- unique(unlist(domain_test_map))

    ## gather all the information into a list
    model_info <- list(
        items   = items,
        cohorts = cohorts,
        domains = domains,
        cohort_domain_map = cohort_domain_map,
        cohort_test_map   = cohort_test_map,
        domain_test_map   = domain_test_map,
        cohort_domain_test_map = cohort_domain_test_map,
        data = data,
        group_var = group_var,
        estimator = "MLR"
    )

    ## assign class name
    class(model_info) <- c("harm_model", "list")

    return(model_info)
}
