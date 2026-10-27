# Boundary and gap labels are interpreted specially by the trajectory engines.
# Validate clinical labels without changing their spelling or punctuation.
.validateStateLabels <- function(labels, argument = "stateCohortLabels") {
  if (is.null(labels)) return(invisible(NULL))
  if (!is.character(labels) || anyNA(labels) || any(!nzchar(trimws(labels)))) {
    stop(argument, " must contain nonempty, non-missing character labels.", call. = FALSE)
  }
  reserved <- labels[toupper(labels) %in% c("0", "START", "EXIT", "OUT OF COHORT")]
  if (length(reserved)) {
    stop(argument, " contains reserved boundary/gap labels: ",
         paste(shQuote(unique(reserved)), collapse = ", "),
         ". Choose different clinical state labels.", call. = FALSE)
  }
  if (anyDuplicated(tolower(labels))) {
    stop(argument, " must be unique, including when ignoring case.", call. = FALSE)
  }
  invisible(NULL)
}

.validateObservedStateLabels <- function(labels, configured = NULL) {
  labels <- as.character(labels)
  # Repeated clinical records are valid. Zero is the target cohort, not a state.
  clinical <- labels[is.na(labels) | labels != "0"]
  .validateStateLabels(unique(c(configured, clinical)), "Clinical state labels")
}
