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

.validateAbsorbingStateReferences <- function(absorbing, states) {
  unknown <- setdiff(as.character(absorbing), as.character(states))
  if (length(unknown)) {
    stop("stateCohortAbsorbing contains unknown state labels: ",
         paste(shQuote(unknown), collapse = ", "),
         ". Use the exact spelling of a configured or observed state, or a named discrete fallback.",
         call. = FALSE)
  }
  invisible(NULL)
}

# Source lookup uses exact names. Duplicate entries would silently select the
# first rule, so reject ambiguity without changing destination semantics.
.validateAllowedStateSources <- function(rules, argument = "allowedStatesList") {
  if (!length(rules)) return(invisible(NULL))
  sources <- names(rules)
  if (is.null(sources) || anyNA(sources) || any(!nzchar(trimws(sources)))) {
    stop(argument, " must name every entry with a nonempty, non-missing source state.",
         call. = FALSE)
  }
  duplicates <- unique(sources[duplicated(sources)])
  if (length(duplicates)) {
    stop(argument, " has duplicate source entries: ",
         paste(shQuote(duplicates), collapse = ", "),
         ". Combine the intended destinations into one entry per source state.",
         call. = FALSE)
  }
  invisible(NULL)
}

# Native engines compare bytes. Use one encoding for their working labels,
# leaving the user's configuration and saved settings unchanged.
.utf8StateLabels <- function(labels) {
  if (is.null(labels)) return(NULL)
  enc2utf8(as.character(labels))
}

.utf8TransitionRules <- function(rules) {
  if (is.null(rules)) return(NULL)
  result <- lapply(rules, .utf8StateLabels)
  names(result) <- .utf8StateLabels(names(rules))
  result
}

.validateGapFallback <- function(value, stateLabels = NULL) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    stop("oocFix must be one nonempty, non-missing string: 'None', ",
         "'Last present state', or a clinical state label.", call. = FALSE)
  }
  # OUT OF COHORT is the existing explicit spelling for an unresolved gap.
  if (!value %in% c("None", "Last present state", "OUT OF COHORT")) {
    .validateStateLabels(value, "oocFix")
    # Adding a fallback must not bypass the case-insensitive uniqueness rule
    # that already applies to recorded and configured clinical labels.
    labels <- as.character(stateLabels)
    conflicts <- unique(labels[which(tolower(labels) == tolower(value) & labels != value)])
    if (length(conflicts)) {
      stop("oocFix ", shQuote(value), " differs only in case from an existing state label: ",
           paste(shQuote(conflicts), collapse = ", "),
           ". Use the exact state label, or choose a distinct fallback label.", call. = FALSE)
    }
  }
  invisible(NULL)
}
