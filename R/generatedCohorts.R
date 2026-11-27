# Own only a unique set of generated tables. Existing cohort tables belong to
# the caller, including a table named 'cohort'. Collect before cleanup and keep
# the caller's connection open on both successful and failed imports.
.collectGeneratedCohorts <- function(cdm, cohortSet, selectedIds) {
  if (!requireNamespace("CirceR", quietly = TRUE) ||
      !requireNamespace("SqlRender", quietly = TRUE)) {
    stop("CDM cohort generation requires loadable CirceR and SqlRender packages. ",
         "Install them with install.packages(c('CirceR', 'SqlRender')); ",
         "if loading still fails, check the Java/rJava configuration.",
         call. = FALSE)
  }
  name <- paste0("c2t_", basename(tempfile(pattern = "")))
  name <- substr(gsub("[^a-z0-9_]", "", tolower(name)), 1L, 13L)
  owned <- paste0(name, c("", "_count", "_set", "_attrition", "_codelist", "_inclusion",
                        "_inclusion_result", "_inclusion_stats", "_summary_stats", "_censor_stats"))
  existing <- omopgenerics::listSourceTables(cdm)
  if (any(tolower(owned) %in% tolower(existing))) {
    stop("Generated cohort table names already exist. Retry the import without changing those tables.",
         call. = FALSE)
  }
  on.exit({
    tryCatch({
      remaining <- intersect(owned, omopgenerics::listSourceTables(cdm))
      if (length(remaining)) omopgenerics::dropSourceTable(cdm, name = remaining)
    }, error = function(e) {
      warning("Could not clean up generated cohort tables with prefix '", name,
              "'. Check the write schema. The caller's existing tables were not selected for removal.",
              call. = FALSE)
    })
  }, add = TRUE)
  generated <- CDMConnector::generateCohortSet(cdm, cohortSet, name = name,
                                              computeAttrition = FALSE, overwrite = FALSE)
  generated[[name]] %>%
    dplyr::filter(.data$cohort_definition_id %in% selectedIds) %>%
    dplyr::arrange(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) %>%
    dplyr::mutate(subject_id = as.character(.data$subject_id)) %>%
    dplyr::collect()
}
