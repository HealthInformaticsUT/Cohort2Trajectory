################################################################################
#
# Some regularly used functions to help code cleanness
#
################################################################################

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
  # Read IDs without type inference or treating a literal "NA" identifier as
  # missing. Other columns retain the usual read.csv type conversion.
  data <- utils::read.csv(path, colClasses = "character", na.strings = NULL)
  data$subject_id <- .simplifySubjectKeys(data$subject_id)
  for (column in setdiff(names(data), "subject_id")) {
    data[[column]] <- utils::type.convert(data[[column]], as.is = TRUE)
  }
  data
}


#' Load and translate SQL file or an explicit SQL query to desired dialect.
#'
#' @param sql SQL file name or SQL query
#' @param warnOnMissingParameters Should a warning be raised when parameters provided to this function do not appear in the parameterized SQL that is being rendered? By default, this is TRUE.
#' @param output Should there be a .sql file created of the result
#' @param outputFile Name of the output file
#' @keywords internal
loadRenderTranslateSql <- function(sql,
                                   dbms = "postgresql",
                                   warnOnMissingParameters = TRUE,
                                   output = FALSE,
                                   outputFile,
                                   ...) {
  if (grepl('.sql', sql)) {
    pathToSql <- paste("inst/SQL/", sql, sep = "")
    parameterizedSql <-
      readChar(pathToSql, file.info(pathToSql)$size)[1]
  }
  else {
    parameterizedSql <- sql
  }
  
  renderedSql <-
    SqlRender::render(sql = parameterizedSql, warnOnMissingParameters = warnOnMissingParameters, ...)
  renderedSql <-
    SqlRender::translate(sql = renderedSql, targetDialect = dbms)
  
  if (output == TRUE) {
    SqlRender::writeSql(renderedSql, outputFile)
    writeLines(paste("Created file '", outputFile, "'", sep = ""))
  }
  
  return(renderedSql)
}

#' Function for finding NaN values in a data.frame object
#'
#' @param data SQL data.frame object
#' @keywords internal
is.nan.data.frame <- function(data) {
  do.call(cbind, lapply(data, is.nan))
}

#' Function for saving summary tables to path
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


#' Function for controlling whether patient exists in a cohort
#'
#' @param data A dataframe object with subject_id values
#' @param id The subject ID
#' @keywords internal
idExists <- function(data, id) {
  if (as.character(id) %in% unique(as.character(data$subject_id)))
    return(TRUE)
  return(FALSE)
}

#' Function for calculating overlap in date intervals
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

#' Function to check if two periods overlap
#'
#' @keywords internal
is_overlap <- function(start1, end1, start2, end2) {
  return(start1 <= end2 && start2 <= end1)
}

#' Function which converts text formatted JSON to digestible JSON
#'
#' @param input Text formatted JSON which needs conversion to digestible JSON
#' @keywords internal
.toJSON <- function(input, pretty = FALSE) {
  return(RJSONIO::toJSON(
    x = input,
    digits = 23,
    pretty = pretty
  ))
}

#' Create the mandatory study subdirectories
#'
#' @param pathToStudy Path to the package 
#' @keywords internal
createMandatorySubDirs <- function(pathToStudy, studyName) {
  
  dir.create(file.path(pathToStudy, studyName), showWarnings = FALSE)
  dir.create(file.path(paste(pathToStudy, '/', studyName, sep = ""), 'JSON'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToStudy, '/', studyName, sep = ""), 'Data'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToStudy, '/', studyName, sep = ""), 'Settings'), showWarnings = FALSE)
  
}



#' Create a list of all states (as elements) for each state (as index)
#'
#' @param stateVector Vector with the defines state labels
#' @export
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
#'   destinations. The input list is not modified in place.
#' @return The updated transition list.
#' @export
removeListVectorEl <-
  function(stateList,
           transitionHead,
           transitionTail) {
    destinations <- stateList[[transitionHead]]
    if (is.null(destinations)) return(stateList)
    stateList[[transitionHead]] <- destinations[!destinations %in% transitionTail]
    return(stateList)
  }


#' Add an allowed transition
#'
#' @param stateList List of all the states storing the possible destination states
#' @param transitionHead State label which is the state where the transition is coming from
#' @param transitionTail State label which is the state where the transition is going to
#' @details Repeated additions do not duplicate destinations. Existing
#'   destination order is preserved and new destinations are appended.
#'   The input list is not modified in place.
#' @return The updated transition list.
#' @export
addListVectorEl <-
  function(stateList,
           transitionHead,
           transitionTail) {
    stateList[[transitionHead]] <- unique(c(stateList[[transitionHead]], transitionTail))
    return(stateList)
  }

#' Create a vector with all possible combinations preserving the priority order
#'
#' @param states Vector of states
#' @param n The largest number of combinations possible
#' @keywords internal
ordered_combinations <- function(states, n) {
  # Generate combinations
  combs <-
    unlist(lapply(1:n, function(x)
      utils::combn(states, x, simplify = FALSE)), recursive = FALSE)
  
  # Generate permutations for each combination
  perms <-
    lapply(combs, function(x)
      gtools::permutations(length(x), length(x), x))
  
  # Concatenate elements of each permutation and collapse list
  result <-
    unlist(lapply(perms, function(x)
      apply(x, 1, paste, collapse = "+")))
  
  # Create a data frame with the numeric order of the first state in each combination
  df <- data.frame(
    result = result,
    first_state = as.numeric(sapply(strsplit(result, "\\+"), function(x)
      which(states == x[1]))),
    count = sapply(strsplit(result, "\\+"), length)
  )
  
  # Sort by the numeric order of the first state
  df <- df[order(df$first_state,-df$count, df$result),]
  return(df$result)
}


#' Function to sanitize a single string
#' @param input_string A  state label name
#' @keywords internal
sanitize_single <- function(input_string) {
  safe_string <- gsub("[^A-Za-z0-9_.-]", "_", input_string)  # Corrected regex
  if (substr(safe_string, 1, 1) == '.') {
    safe_string <- paste0('_', safe_string)
  }
  return(safe_string)
}

#' Sanitize filenames not in correct format
#'
#' @param input_strings A vector of state label names
#' @keywords internal
sanitize_filenames <- function(input_strings) {
  # Apply the sanitization function to each element of the vector
  sapply(input_strings, sanitize_single)
}

#' Custom function to merge overlapping date ranges within each group
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
