#' @importFrom dplyr all_of matches starts_with contains ends_with
#' @importFrom rlang :=
#' @importFrom Rcpp evalCpp
#' @useDynLib Cohort2Trajectory, .registration=TRUE
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom stats na.exclude
#' @importFrom utils combn
utils::globalVariables(".")
