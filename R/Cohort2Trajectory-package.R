#' Create patient trajectories from clinical cohorts
#'
#' Turn analyst-defined cohort intervals into discrete cycles or continuous
#' histories. See the package vignettes for a local example and database setup.
#'
#' @importFrom dplyr all_of matches starts_with contains ends_with
#' @importFrom rlang :=
#' @importFrom Rcpp evalCpp
#' @useDynLib Cohort2Trajectory, .registration=TRUE
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom stats na.exclude
#' @importFrom utils combn
"_PACKAGE"

utils::globalVariables(".")
