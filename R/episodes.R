# Assign observation episodes before eligibility filtering. Only intersections
# with target intervals are retained, so gaps never become observed time.
.cleanAllTargetEpisodes <- function(cohortData, mandatoryStates) {
  columns <- c("subject_id", "cohort_definition_id", "cohort_start_date", "cohort_end_date")
  if (!all(columns %in% names(cohortData))) {
    stop("Cohort data must contain: ", paste(columns, collapse = ", "), ".", call. = FALSE)
  }
  data <- as.data.frame(cohortData[, columns, drop = FALSE])
  keys <- .subjectKeys(data$subject_id)
  data$cohort_definition_id <- as.character(data$cohort_definition_id)
  .validateObservedStateLabels(data$cohort_definition_id)
  data$cohort_start_date <- as.Date(data$cohort_start_date)
  data$cohort_end_date <- as.Date(data$cohort_end_date)
  if (anyNA(data$cohort_start_date) || anyNA(data$cohort_end_date) ||
      any(data$cohort_end_date < data$cohort_start_date)) {
    stop("Cohort intervals must have non-missing dates with end >= start.", call. = FALSE)
  }

  targets <- data[data$cohort_definition_id == "0", , drop = FALSE]
  targetKeys <- keys[data$cohort_definition_id == "0"]
  keep <- !duplicated(data.frame(subject = targetKeys,
                                start = targets$cohort_start_date,
                                end = targets$cohort_end_date))
  targets <- targets[keep, , drop = FALSE]
  targetKeys <- targetKeys[keep]
  order <- order(targetKeys, targets$cohort_start_date, targets$cohort_end_date,
                 method = "radix")
  targets <- targets[order, , drop = FALSE]
  targetKeys <- targetKeys[order]
  targets$episode_id <- integer(nrow(targets))
  groups <- split(seq_len(nrow(targets)), targetKeys)
  for (indices in groups) {
    if (length(indices) > 1L && any(
      targets$cohort_start_date[indices[-1L]] <=
        targets$cohort_end_date[indices[-length(indices)]]
    )) {
      stop("Target intervals overlap for subject_id ", shQuote(targetKeys[indices[1L]]),
           ". All-target episodes must have disjoint inclusive dates.", call. = FALSE)
    }
    targets$episode_id[indices] <- seq_along(indices)
  }

  states <- data[data$cohort_definition_id != "0", , drop = FALSE]
  stateGroups <- split(seq_len(nrow(states)), keys[data$cohort_definition_id != "0"])
  stateGroupIndex <- match(targetKeys, names(stateGroups))
  rows <- list(targets)
  for (i in seq_len(nrow(targets))) {
    if (is.na(stateGroupIndex[i])) next
    indices <- stateGroups[[stateGroupIndex[i]]]
    indices <- indices[
      states$cohort_start_date[indices] <= targets$cohort_end_date[i] &
        states$cohort_end_date[indices] >= targets$cohort_start_date[i]
    ]
    if (!length(indices)) next
    fragment <- states[indices, , drop = FALSE]
    fragment$cohort_start_date <- pmax(fragment$cohort_start_date, targets$cohort_start_date[i])
    fragment$cohort_end_date <- pmin(fragment$cohort_end_date, targets$cohort_end_date[i])
    fragment$episode_id <- targets$episode_id[i]
    rows[[length(rows) + 1L]] <- fragment
  }
  bounded <- dplyr::bind_rows(rows)
  encoded <- .encodeSubjectIds(bounded)
  encoded$data$episode_id <- NULL
  cleaned <- cleanCohortData(encoded$data, mandatoryStates, outOfCohortAllowed = FALSE)
  .restoreSubjectIds(cleaned, encoded$originals)
}
