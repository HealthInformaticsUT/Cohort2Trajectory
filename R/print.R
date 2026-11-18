#' Print a study or trajectory preview
#'
#' Display a compact summary without changing study settings or trajectory
#' data. A study remains an environment, and trajectories remain a data frame.
#'
#' @param x A study returned by \code{cohort2TrajectoryConfiguration()} or
#'   trajectories returned by \code{createTrajectories()}.
#' @param ... Further arguments passed to the data-frame preview's print method.
#'   Unused when printing a study.
#' @param n Maximum number of trajectory rows to preview. A non-negative whole
#'   number; defaults to six.
#'
#' @details
#' The trajectory summary counts patients and distinct clinical labels present
#' in the returned rows. \code{START} and \code{EXIT} are boundary rows;
#' \code{OUT OF COHORT} rows are unresolved cycles. These labels are excluded
#' from the clinical-state count. Merged labels count as distinct states.
#'
#' The preview shows patient identifiers, state labels, start and end dates,
#' and time in cohort. All columns remain available through normal data-frame
#' operations. Use \code{as.data.frame(x)} for the ordinary full
#' data-frame display or \code{View(x)} to inspect the table. Column subsets
#' without the trajectory's patient, label or date columns use ordinary
#' data-frame printing.
#'
#' Inspect a study's individual settings with \code{x$settingName}, or all
#' settings with \code{as.list(x, all.names = TRUE)}. Printing does not read
#' patient files or change the saved study.
#'
#' @return The supplied object, invisibly.
#' @name print.c2t_study
#' @export
print.c2t_study <- function(x, ...) {
  choice <- function(value) {
    if (is.null(value)) "not set" else paste(value, collapse = ", ")
  }
  source <- if (isTRUE(x$useCDM)) {
    "CDM"
  } else if (is.data.frame(x$trajectoryDataObject)) {
    "local data frame"
  } else {
    "local CSV"
  }
  cat("<Cohort2Trajectory study>\n")
  cat("Study:", choice(x$studyName), "| Source:", source, "\n")
  cat("Trajectory:", choice(x$trajectoryType),
      "| Selection:", choice(x$stateSelectionType))
  if (identical(x$trajectoryType, "Discrete")) {
    cat(" | Cycle:", choice(x$cycleLength), "days")
  }
  cat("\nClinical states configured:", length(x$stateCohortLabels), "\n")
  cat("Folder:", file.path(x$pathToStudy, x$studyName), "\n")
  cat("Inspect all settings with as.list(x, all.names = TRUE).\n")
  invisible(x)
}

#' @rdname print.c2t_study
#' @export
print.c2t_trajectories <- function(x, ..., n = 6L) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
      n < 0 || n != trunc(n)) {
    stop("n must be a single non-negative whole number.", call. = FALSE)
  }
  required <- c("subject_id", "state_label", "state_start_date", "state_end_date")
  if (!all(required %in% names(x))) {
    NextMethod("print")
    return(invisible(x))
  }
  labels <- as.character(x$state_label)
  boundaries <- labels %in% c("START", "EXIT")
  unresolved <- labels %in% "OUT OF COHORT"
  clinical <- !is.na(labels) & !boundaries & !unresolved & labels != "0"
  cat("<Cohort2Trajectory trajectories>\n")
  cat("Patients:", length(unique(x$subject_id[!is.na(x$subject_id)])),
      "| Clinical states present:", length(unique(labels[clinical])),
      "| Rows:", nrow(x), "\n")
  cat("Boundary rows (START/EXIT):", sum(boundaries),
      "| Unresolved rows (OUT OF COHORT):", sum(unresolved), "\n")
  columns <- intersect(c("subject_id", "state_label", "state_start_date",
                         "state_end_date", "time_in_cohort"), names(x))
  rows <- seq_len(min(n, nrow(x)))
  cat("Preview:", length(rows), "of", nrow(x), "rows;",
      length(columns), "of", ncol(x), "columns\n")
  print(as.data.frame(x)[rows, columns, drop = FALSE], ...)
  cat("Use as.data.frame(x) to inspect every column.\n")
  invisible(x)
}
