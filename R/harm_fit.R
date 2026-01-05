#' Define a Harmonization Fit Specification
#'
#' This function creates a 'harm_fit' object that stores the harmonized parameters of
#' a `harm_model` for furthur predictions.
#'
#' @param model A `harm_model` object representing model structure.
#' @param cohort_domain_test_map A map showing which tests belongs to each cohort's
#'      each domain.
#' @param covariate_lst A list showing which ethnics info is presented in each cohort
#' @param preferred_domain_order A vector show preferred order of domains
#' @param data A named list, store the raw data by cohort.
#' @param group_var A string, the name of the grouping variable. Default to 'cohort'.
#'
#' @return An object of class 'harm_model'.
#' @export
