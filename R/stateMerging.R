#' Override the behaviour of one merged state
#'
#' @param members Character vector of original state labels. Order is immaterial.
#' @param allowedTransitions Optional replacement destinations. A character vector
#'   names individual states. A list can also specify composite destinations,
#'   for example list("C", c("C", "D")). character(0) permits no destinations.
#'   Omit this argument to inherit transitions.
#' @param absorbing Optional TRUE or FALSE replacing inherited absorbing status.
#' @param label Optional display label. It is never parsed to determine membership.
#' @return A definition for mergedStateOverrides in the study configuration.
#' @export
#' @examples
#' mergedState(c("A", "B"), allowedTransitions = "C", absorbing = FALSE)
#' mergedState(c("A", "B"), absorbing = FALSE)
mergedState <- function(members, allowedTransitions, absorbing, label = NULL) {
  members <- .stateMembers(members)
  if (length(members) < 2L) stop("A merged state needs at least two distinct members.", call. = FALSE)
  result <- list(members = members, label = label)
  if (!is.null(label) && (!is.character(label) || length(label) != 1L ||
                        is.na(label) || !nzchar(trimws(label)))) {
    stop("label must be one nonempty string.", call. = FALSE)
  }
  if (!missing(allowedTransitions)) {
    result$allowedTransitions <- .destinationSets(allowedTransitions)
  }
  if (!missing(absorbing)) {
    if (!is.logical(absorbing) || length(absorbing) != 1L || is.na(absorbing)) {
      stop("absorbing must be TRUE or FALSE. Omit it to inherit.", call. = FALSE)
    }
    result$absorbing <- absorbing
  }
  structure(result, class = "c2t_merged_state")
}

.stateMembers <- function(x) {
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
    stop("State members must be nonempty character labels.", call. = FALSE)
  }
  sort(unique(enc2utf8(x)), method = "radix")
}

.destinationSets <- function(x) {
  if (is.null(x) || !(is.character(x) || is.list(x))) {
    stop("allowedTransitions must be a character vector or list of member vectors. Omit it to inherit.", call. = FALSE)
  }
  lapply(if (is.character(x)) as.list(x) else x, .stateMembers)
}

# A collision-free, deterministic key for a set. Membership is stored separately
# and never recovered by splitting this key or the display label.
.stateKey <- function(members) {
  members <- .stateMembers(members)
  paste0(if (length(members) == 1L) "s:" else "m:",
         paste0(nchar(members, type = "bytes"), ":", members, collapse = ""))
}

.legacyMergeArguments <- function(dots) {
  if (!length(dots)) return(invisible(NULL))
  if (is.null(names(dots)) || any(!names(dots) %in% "mergeThreshold")) {
    stop("Unknown argument(s): ", paste(names(dots), collapse = ", "), call. = FALSE)
  }
  warning("mergeThreshold has been removed and is ignored. It never affected the previous overlap calculation. Merging now uses exact interval overlap.", call. = FALSE)
}

.validateMergeOptions <- function(mergeStates, overrides) {
  if (!is.logical(mergeStates) || length(mergeStates) != 1L || is.na(mergeStates)) {
    stop("mergeStates must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.list(overrides) || any(!vapply(overrides, inherits, logical(1), "c2t_merged_state"))) {
    stop("mergedStateOverrides must be a list of mergedState() definitions.", call. = FALSE)
  }
  if (length(overrides) && !mergeStates) {
    stop("Set mergeStates = TRUE to use mergedStateOverrides.", call. = FALSE)
  }
  keys <- vapply(overrides, function(x) .stateKey(x$members), character(1))
  if (anyDuplicated(keys)) stop("Only one override is allowed per membership set.", call. = FALSE)
}

.stateRegistry <- function(baseLabels, memberships = list(), overrides = list()) {
  baseLabels <- unique(as.character(baseLabels))
  if (length(baseLabels)) .stateMembers(baseLabels)
  if (anyDuplicated(tolower(baseLabels)) ||
      any(baseLabels %in% c("0", "START", "EXIT", "OUT OF COHORT"))) {
    stop("Merged studies need distinct state labels (also ignoring case), excluding reserved boundary/gap labels.", call. = FALSE)
  }
  sets <- c(as.list(baseLabels), memberships, lapply(overrides, `[[`, "members"),
            unlist(lapply(overrides, function(x) x$allowedTransitions), recursive = FALSE))
  sets <- lapply(sets, .stateMembers)
  unknown <- setdiff(unique(unlist(sets)), baseLabels)
  if (length(unknown)) stop("Unknown state members: ", paste(unknown, collapse = ", "), call. = FALSE)
  keys <- vapply(sets, .stateKey, character(1))
  sets <- sets[!duplicated(keys)]
  keys <- keys[!duplicated(keys)]
  order <- order(lengths(sets) > 1L, keys, method = "radix")
  sets <- sets[order]
  keys <- keys[order]
  labels <- vapply(sets, paste, character(1), collapse = " + ")
  custom <- rep(FALSE, length(keys))
  for (x in overrides) if (!is.null(x$label)) {
    i <- match(.stateKey(x$members), keys)
    labels[i] <- x$label
    custom[i] <- TRUE
  }
  # Original and explicitly named states retain their labels. Disambiguate only
  # generated display labels, without affecting identity or membership.
  fixed <- lengths(sets) == 1L | custom
  if (anyDuplicated(tolower(labels[fixed]))) stop("Custom merged labels must be unique.", call. = FALSE)
  occupied <- tolower(labels[fixed])
  for (i in which(!fixed)) {
    if (tolower(labels[i]) %in% occupied) labels[i] <- paste0(labels[i], " [", keys[i], "]")
    while (tolower(labels[i]) %in% occupied) labels[i] <- paste0(labels[i], " [combined]")
    occupied <- c(occupied, tolower(labels[i]))
  }
  result <- data.frame(state_key = keys, label = labels, stringsAsFactors = FALSE)
  result$members <- I(sets)
  result
}

# Sweep changes in active membership, rather than expanding every patient-day.
# Inputs have already been bounded and filtered for mandatory original states.
.composeStateIntervals <- function(data, baseLabels, overrides) {
  original <- as.character(data$cohort_definition_id)
  baseLabels <- unique(c(baseLabels, original[original != "0"]))
  .stateRegistry(baseLabels, overrides = overrides) # Validate before composing.
  states <- data[original != "0", , drop = FALSE]
  targets <- data[original == "0", , drop = FALSE]
  groups <- split(seq_len(nrow(states)), .subjectKeys(states$subject_id))
  rows <- list()
  memberships <- new.env(hash = TRUE, parent = emptyenv())
  for (indices in groups) {
    patient <- states[indices, , drop = FALSE]
    stateIds <- match(patient$cohort_definition_id, baseLabels)
    times <- c(as.numeric(patient$cohort_start_date), as.numeric(patient$cohort_end_date) + 1)
    eventIds <- rep(stateIds, 2L)
    deltas <- rep(c(1L, -1L), each = nrow(patient))
    eventOrder <- order(times)
    times <- times[eventOrder]
    eventIds <- eventIds[eventOrder]
    deltas <- deltas[eventOrder]
    starts <- which(!duplicated(times))
    counts <- integer(length(baseLabels))
    for (j in seq_len(length(starts) - 1L)) {
      event <- starts[j]:(starts[j + 1L] - 1L)
      counts <- counts + tabulate(eventIds[event][deltas[event] == 1L], length(baseLabels)) -
        tabulate(eventIds[event][deltas[event] == -1L], length(baseLabels))
      active <- baseLabels[counts > 0L]
      if (!length(active)) next
      key <- .stateKey(active)
      memberships[[key]] <- .stateMembers(active)
      row <- patient[1L, , drop = FALSE]
      row$cohort_definition_id <- key
      row$cohort_start_date <- as.Date(times[starts[j]], origin = "1970-01-01")
      row$cohort_end_date <- as.Date(times[starts[j + 1L]] - 1, origin = "1970-01-01")
      last <- length(rows)
      if (last && identical(.subjectKeys(rows[[last]]$subject_id), .subjectKeys(row$subject_id)) &&
          rows[[last]]$cohort_definition_id == key &&
          rows[[last]]$cohort_end_date + 1 == row$cohort_start_date) {
        rows[[last]]$cohort_end_date <- row$cohort_end_date
      } else rows[[last + 1L]] <- row
    }
  }
  registry <- .stateRegistry(baseLabels, as.list(memberships), overrides)
  resolved <- dplyr::bind_rows(c(list(targets), rows))
  reference <- targets$cohort_start_date[match(.subjectKeys(resolved$subject_id), .subjectKeys(targets$subject_id))]
  resolved$time_in_cohort <- as.numeric(resolved$cohort_start_date - reference) / 365.25
  list(data = resolved, registry = registry)
}

.mergedRules <- function(registry, priority, rules, absorbing, overrides) {
  baseLabels <- vapply(registry$members[lengths(registry$members) == 1L], `[[`, character(1), 1L)
  registry <- .stateRegistry(baseLabels, registry$members, overrides)
  if (is.null(priority)) priority <- baseLabels
  missing <- setdiff(baseLabels, priority)
  if (length(missing)) stop("States missing from stateCohortPriorityOrder: ", paste(missing, collapse = ", "), call. = FALSE)
  unknown <- setdiff(c(priority, absorbing, names(rules)), baseLabels)
  if (length(unknown)) stop("Use mergedStateOverrides for composite rules. Unknown original states: ", paste(unknown, collapse = ", "), call. = FALSE)
  if (length(rules) && (is.null(names(rules)) || anyDuplicated(names(rules)) ||
                       !all(baseLabels %in% names(rules)))) {
    stop("Provide one allowedStatesList entry per original state, or list() for unrestricted transitions.", call. = FALSE)
  }
  baseDestinations <- lapply(rules, function(x) {
    if (is.null(x)) x <- character(0)
    if (!is.character(x) || anyNA(x)) stop("Allowed destinations must be character labels.", call. = FALSE)
    matched <- match(tolower(x), tolower(baseLabels))
    if (anyNA(matched)) stop("Unknown allowed destination. Use mergedStateOverrides for composite destinations.", call. = FALSE)
    baseLabels[matched]
  })
  overrideKeys <- vapply(overrides, function(x) .stateKey(x$members), character(1))
  resolved <- stats::setNames(vector("list", nrow(registry)), registry$state_key)
  terminal <- logical(nrow(registry))
  for (i in seq_len(nrow(registry))) {
    members <- registry$members[[i]]
    index <- match(registry$state_key[i], overrideKeys)
    override <- if (!is.na(index)) overrides[[index]] else list()
    terminal[i] <- if ("absorbing" %in% names(override)) override$absorbing else any(members %in% absorbing)
    if ("allowedTransitions" %in% names(override)) {
      resolved[[i]] <- vapply(override$allowedTransitions, .stateKey, character(1))
    } else {
      permitted <- if (!length(rules)) baseLabels else unique(unlist(baseDestinations[members]))
      allowed <- vapply(registry$members, function(destination) {
        added <- setdiff(destination, members)
        # Existing members need no new entry permission. A self-transition or
        # pure removal still requires permission for the retained destination.
        all((if (length(added)) added else destination) %in% permitted)
      }, logical(1))
      resolved[[i]] <- registry$state_key[allowed]
    }
  }
  ranks <- vapply(registry$members, function(x) min(match(x, priority)), numeric(1))
  priorityKeys <- registry$state_key[order(ranks, -lengths(registry$members), registry$state_key)]
  registry$absorbing <- terminal
  registry$allowedTransitions <- I(unname(resolved))
  list(registry = registry, priority = priorityKeys, allowed = resolved,
       absorbing = registry$state_key[terminal])
}
