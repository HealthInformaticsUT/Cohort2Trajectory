# Resolve priority and transition eligibility together at interval boundaries.
# Inputs are already bounded. No patient-day expansion or gap filling is needed.
.selectContinuousPriorityIntervals <- function(data, priority, rules, absorbing) {
  boundaries <- data[data$cohort_definition_id %in% c("START", "EXIT"), , drop = FALSE]
  states <- data[!data$cohort_definition_id %in% c("START", "EXIT"), , drop = FALSE]
  missing <- setdiff(unique(states$cohort_definition_id), priority)
  if (length(missing)) {
    stop("States missing from statePriorityVector: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  rows <- list()
  groups <- split(seq_len(nrow(states)), states$subject_id)
  for (indices in groups) {
    patient <- states[indices, , drop = FALSE]
    labels <- patient$cohort_definition_id
    starts <- as.numeric(patient$cohort_start_date)
    ends <- as.numeric(patient$cohort_end_date)
    ranks <- match(labels, priority)
    changes <- sort(unique(c(starts, ends + 1)))
    selected <- NA_integer_
    lastState <- NULL
    for (j in seq_len(length(changes) - 1L)) {
      start <- changes[j]
      end <- changes[j + 1L] - 1
      active <- which(starts <= start & ends >= start)
      # Prefer continuing the same record over an equal-priority alternative.
      ongoing <- if (is.na(selected)) rep(FALSE, length(active)) else active == selected
      active <- active[order(ranks[active], !ongoing)]
      chosen <- NA_integer_
      for (candidate in active) {
        if (identical(candidate, selected) || is.null(lastState) || !length(rules)) {
          chosen <- candidate
          break
        }
        if (!lastState %in% names(rules)) {
          stop("No allowed-transition entry for state '", lastState, "'.", call. = FALSE)
        }
        if (tolower(labels[candidate]) %in% tolower(rules[[lastState]])) {
          chosen <- candidate
          break
        }
      }
      if (is.na(chosen)) {
        # An unresolved gap does not reset the last retained clinical state.
        selected <- NA_integer_
        next
      }
      terminal <- labels[chosen] %in% absorbing
      # Once selected, an absorbing interval keeps its bounded recorded end.
      # Later arrivals cannot shorten it or require an outgoing transition.
      if (terminal) end <- ends[chosen]
      if (identical(chosen, selected)) {
        rows[[length(rows)]]$cohort_end_date <- as.Date(end, origin = "1970-01-01")
      } else {
        row <- patient[chosen, , drop = FALSE]
        row$cohort_start_date <- as.Date(start, origin = "1970-01-01")
        row$cohort_end_date <- as.Date(end, origin = "1970-01-01")
        rows[[length(rows) + 1L]] <- row
      }
      selected <- chosen
      lastState <- labels[chosen]
      if (terminal) break
    }
  }
  dplyr::bind_rows(c(list(boundaries), rows))
}
