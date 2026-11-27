# Match identifiers as exact text, never by narrowing them to native integers.
.subjectKeys <- function(ids) {
  if (inherits(ids, "integer64") || is.character(ids) || is.factor(ids)) {
    keys <- as.character(ids)
  } else if (is.numeric(ids) && !is.complex(ids)) {
    if (any(!is.finite(ids)) || any(ids != trunc(ids))) {
      stop("subject_id must contain non-missing whole numbers or text identifiers.", call. = FALSE)
    }
    if (any(abs(ids) >= 2^53)) {
      stop("subject_id supplied as double may already be rounded. Supply large IDs as character or integer64, not double.",
           call. = FALSE)
    }
    keys <- sprintf("%.0f", ids)
  } else {
    stop("subject_id must contain numeric, character or integer64 identifiers.", call. = FALSE)
  }
  if (anyNA(keys) || any(!nzchar(trimws(keys)))) {
    stop("subject_id must not contain missing or blank identifiers.", call. = FALSE)
  }
  keys
}

.encodeSubjectIds <- function(data) {
  keys <- .subjectKeys(data$subject_id)
  first <- !duplicated(keys)
  originals <- data$subject_id[first]
  if (length(originals) > .Machine$integer.max) {
    stop("Too many distinct patients in one native operation.", call. = FALSE)
  }
  data$subject_id <- match(keys, keys[first])
  list(data = data, originals = originals)
}

.restoreSubjectIds <- function(data, originals) {
  data$subject_id <- originals[as.integer(data$subject_id)]
  data
}

# Keep the historical numeric output for canonical small numeric IDs only.
# Large IDs, leading zeros and nonnumeric labels must stay as exact text.
.simplifySubjectKeys <- function(ids) {
  keys <- .subjectKeys(ids)
  numbers <- suppressWarnings(as.numeric(keys))
  if (all(is.finite(numbers)) && all(abs(numbers) < 2^53) &&
      all(numbers == trunc(numbers)) && identical(sprintf("%.0f", numbers), keys)) {
    return(numbers)
  }
  keys
}

.readPatientCsv <- function(path) {
  # Patient IDs and clinical labels are identifiers, not numbers. Preserve
  # leading zeros and literal "NA" text in both. Only other columns undergo
  # type inference. Blank clinical labels are rejected by import validation.
  data <- utils::read.csv(path, colClasses = "character", na.strings = NULL)
  data$subject_id <- .simplifySubjectKeys(data$subject_id)
  for (column in setdiff(names(data), c("subject_id", "cohort_definition_id"))) {
    data[[column]] <- utils::type.convert(data[[column]], as.is = TRUE)
  }
  data
}


#' Save a table as CSV or another object as an R data file
#'
#' @param object Object to save
#' @param path Path to the file saved
#' @keywords internal
save_object <- function(object, path) {
  if (is.data.frame(object)) {
    if ("subject_id" %in% names(object)) {
      object$subject_id <- .subjectKeys(object$subject_id)
    }
    utils::write.csv(object, path, row.names = FALSE)
  }
  else {
    save(object, file = path)
  }
}


#' Count shared days between two inclusive date intervals
#'
#' @param dateStart Start of the boundary date interval
#' @param dateEnd End of the boundary date interval
#' @param controlStart Start of the date interval of interest
#' @param controlEnd End of the date interval of interest
#' @keywords internal
daysOverlap <- function(dateStart,
                        dateEnd,
                        controlStart,
                        controlEnd) {
  overlapStart <- max(dateStart, controlStart)
  overlapEnd <- min(dateEnd, controlEnd)
  max(as.numeric(overlapEnd - overlapStart) + 1, 0)
}

#' Check whether two inclusive date intervals overlap
#'
#' @keywords internal
is_overlap <- function(start1, end1, start2, end2) {
  return(start1 <= end2 && start2 <= end1)
}

#' Serialize a cohort expression as JSON
#'
#' @param input An R object containing a cohort expression.
#' @param pretty Whether to indent the JSON output.
#' @keywords internal
.toJSON <- function(input, pretty = FALSE) {
  as.character(jsonlite::toJSON(input, auto_unbox = TRUE, null = "null",
                                digits = NA, pretty = pretty))
}

#' Create the mandatory study subdirectories
#'
#' @param pathToStudy Parent folder for the study.
#' @param studyName Name of the study folder.
#' @keywords internal
createMandatorySubDirs <- function(pathToStudy, studyName) {
  .validateStudyPath(pathToStudy, studyName)
  paths <- file.path(pathToStudy, studyName, c("", "JSON", "Data", "Settings"))
  for (path in paths) {
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create study directory: ", path, call. = FALSE)
    }
  }
  invisible(NULL)
}

.validateStudyPath <- function(pathToStudy, studyName) {
  if (!is.character(pathToStudy) || length(pathToStudy) != 1L ||
      is.na(pathToStudy) || !nzchar(trimws(pathToStudy))) {
    stop("pathToStudy must be one nonempty folder path.", call. = FALSE)
  }
  if (!is.character(studyName) || length(studyName) != 1L || is.na(studyName) ||
      !nzchar(trimws(studyName)) || studyName %in% c(".", "..") ||
      grepl("[/\\\\]", studyName)) {
    stop("studyName must be one folder name without path separators.", call. = FALSE)
  }
  invisible(NULL)
}

# Local definitions can have nested paths. The cleaned-data artifact has one
# stable filename within its study and never inherits a definition path.
.cleanedDataPath <- function(pathToStudy, studyName) {
  file.path(pathToStudy, studyName, "Data", "importedDataCleaned.csv")
}


#' Allow every state to transition to every state
#'
#' @param stateVector Character vector of state labels.
#' @return A named list allowing every state to transition to every state.
#' @export
#' @examples
#' createStateList(c("Stable", "Complication", "Death"))
createStateList <- function(stateVector) {
  stateList = replicate(n = length(stateVector),
                        expr = stateVector,
                        simplify = FALSE)
  names(stateList) = stateVector
  return(stateList)
}


#' Remove an allowed transition
#'
#' @param stateList List of all the states storing the possible destination states
#' @param transitionHead State label which is the state where the transition is coming from
#' @param transitionTail State label which is the state where the transition is going to
#' @details Removing an absent transition leaves the list unchanged. Removing
#'   the last destination preserves an empty source entry, meaning no allowed
#'   destinations. Destination matching is case-insensitive using R's
#'   locale-aware case conversion, as in trajectory generation. All matching
#'   entries are removed. Source names still require exact spelling. The input
#'   list is not modified in place.
#' @return The updated transition list.
#' @export
#' @examples
#' rules <- createStateList(c("Stable", "Complication", "Death"))
#' removeListVectorEl(rules, "Complication", "Stable")
removeListVectorEl <-
  function(stateList,
           transitionHead,
           transitionTail) {
    destinations <- stateList[[transitionHead]]
    if (is.null(destinations)) return(stateList)
    stateList[[transitionHead]] <- destinations[!tolower(destinations) %in% tolower(transitionTail)]
    return(stateList)
  }


#' Add an allowed transition
#'
#' @param stateList List of all the states storing the possible destination states
#' @param transitionHead State label which is the state where the transition is coming from
#' @param transitionTail State label which is the state where the transition is going to
#' @details Destinations are deduplicated case-insensitively using R's
#'   locale-aware case conversion, as in trajectory generation. The first
#'   spelling and order are preserved and new destinations are appended.
#'   Source names still require exact spelling. The input list is not modified
#'   in place.
#'   This helper edits the list without checking study membership. Trajectory
#'   generation rejects destinations that are unknown to the study.
#' @return The updated transition list.
#' @export
#' @examples
#' rules <- list(Stable = "Stable", Complication = "Complication")
#' addListVectorEl(rules, "Stable", "Complication")
addListVectorEl <-
  function(stateList,
           transitionHead,
           transitionTail) {
    destinations <- c(stateList[[transitionHead]], transitionTail)
    stateList[[transitionHead]] <- destinations[!duplicated(tolower(destinations))]
    return(stateList)
  }



#' Merge overlapping date intervals within one patient and state
#'
#' @param data A dataframe with raw cohort data
#' @keywords internal
merge_overlaps <- function(data) {
  data <- data[order(data$cohort_start_date), ]
  i <- 1
  while(i < nrow(data)) {
    # Check if current row overlaps with the next one
    if(is_overlap(data$cohort_start_date[i], data$cohort_end_date[i], data$cohort_start_date[i+1], data$cohort_end_date[i+1])) {
      # Merge the date ranges
      data$cohort_start_date[i] <- min(data$cohort_start_date[i], data$cohort_start_date[i+1])
      data$cohort_end_date[i] <- max(data$cohort_end_date[i], data$cohort_end_date[i+1])
      # Remove the next row since it's been merged
      data <- data[-(i+1), ]
    } else {
      # Move to the next row if no overlap
      i <- i + 1
    }
  }
  return(data)
}
