################################################################################
#
# Some regularly used functions to help code cleanness
#
################################################################################


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
    utils::write.csv(object, path, row.names = FALSE)
  }
  else {
    save(object, file = path)
  }
}


#' Function for controlling whether patient exists in a cohort
#'
#' @param data A dataframe object with SUBJECT_ID values
#' @param id The subject ID
#' @keywords internal
idExists <- function(data, id) {
  if (as.character(id) %in% unique(as.character(data$SUBJECT_ID)))
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
  if (dateStart > controlStart &
      dateEnd > controlEnd) {
    return(max(as.numeric(controlEnd - dateStart), 0))
  }
  else if (dateStart <= controlStart &
           dateEnd > controlEnd) {
    return(max(as.numeric(controlEnd - controlStart) + 1, 0))
  }
  else if (dateStart <= controlStart &
           dateEnd <= controlEnd) {
    return(max(as.numeric(dateEnd - controlStart) + 1, 0))
  }
  else if (dateStart > controlStart &
           dateEnd <= controlEnd) {
    return(max(as.numeric(dateEnd - dateStart), 0))
  }
  else {
    return(0)
  }
}

#' Function to check if two periods overlap
#'
#' @keywords internal
is_overlap <- function(start1, end1, start2, end2) {
  return(start1 <= end2 && start2 <= end1)
}


#' Function for deleting temporary tables from user's db
#'
#' @param connection Connection to the database (package DatabaseConnector)
#' @param dbms Database dialect
#' @param schema Schema in which the targeted table resides
#' @param relationName Name of the targeted table which will be dropped
#' @keywords internal
dropRelation <-
  function(connection,
           dbms = "postgresql",
           schema = "",
           relationName) {
    ParallelLogger::logInfo(paste(
      "Start execution of: DROP TABLE IF EXISTS ",
      ifelse(
        schema == "",
        relationName,
        paste(schema,
              ".",
              relationName, sep = "")
      ),
      " !",
      sep = ""
    ))
    if (schema == "") {
      DatabaseConnector::executeSql(connection,
                                    SqlRender::translate(
                                      targetDialect = dbms,
                                      sql = SqlRender::render(sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @relationName;",
                                                              relationName = relationName)
                                    ))
    }
    else {
      DatabaseConnector::executeSql(connection,
                                    SqlRender::translate(
                                      targetDialect = dbms,
                                      sql = SqlRender::render(
                                        sql = "IF OBJECT_ID('table', 'U') IS NOT NULL DROP TABLE @cdmTmpSchema.@relationName;",
                                        cdmTmpSchema = schema,
                                        relationName = relationName
                                      )
                                    ))
    }
    
    ParallelLogger::logInfo(paste("DROP TABLE ",
                                  schema,
                                  ".",
                                  relationName,
                                  " EXECUTED!",
                                  sep = ""))
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

#' Function which creates mandatory subdirectories and files to the pathToResults directory
#'
#' @param pathToResults Path to the package results
#' @keywords internal
createMandatorySubDirs <- function(pathToResults) {
  dir.create(file.path(pathToResults, "tmp"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/tmp', sep = ""), 'datasets'), showWarnings = FALSE)
  
  dir.create(file.path(pathToResults, "inst"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'JSON'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'SQL'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/inst', sep = ""), 'Settings'), showWarnings = FALSE)
  file.copy(
    paste(getwd(), '/inst/Settings/trajectorySettings.csv', sep = ""),
    paste(pathToResults, '/inst/Settings', sep = "")
  )
}



#' Create a list of all states (as elements) for each state (as index)
#'
#' @param stateVector Vector with the defines state labels
#' @keywords internal
createStateList <- function(stateVector) {
  stateList = replicate(n = length(stateVector),
                        expr = stateVector,
                        simplify = FALSE)
  names(stateList) = stateVector
  return(stateList)
}


#' Remove element from list element's vector
#'
#' @param stateList List of all the states storing the possible destination states
#' @param transitionHead State label which is the state where the transition is coming from
#' @param transitionTail State label which is the state where the transition is going to
#' @keywords internal
removeListVectorEl <-
  function(stateList,
           transitionHead,
           transitionTail) {
    stateList[[transitionHead]] = stateList[[transitionHead]][-which(stateList[[transitionHead]] == transitionTail)]
    return(stateList)
  }


#' Add element from list element's vector
#'
#' @param stateList List of all the states storing the possible destination states
#' @param transitionHead State label which is the state where the transition is coming from
#' @param transitionTail State label which is the state where the transition is going to
#' @keywords internal
addListVectorEl <-
  function(stateList,
           transitionHead,
           transitionTail) {
    stateList[[transitionHead]] = c(stateList[[transitionHead]], transitionTail)
    return(stateList)
  }


# Load required library
library(gtools)

#' Create a vector with all possible combinations preserving the priority order
#'
#' @param states Vector of states
#' @param n The largest number of combinations possible
#' @keywords internal
ordered_combinations <- function(states, n) {
  # Generate combinations
  combs <-
    unlist(lapply(1:n, function(x)
      combn(states, x, simplify = FALSE)), recursive = FALSE)
  
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
  data <- data[order(data$COHORT_START_DATE), ]
  i <- 1
  while(i < nrow(data)) {
    # Check if current row overlaps with the next one
    if(is_overlap(data$COHORT_START_DATE[i], data$COHORT_END_DATE[i], data$COHORT_START_DATE[i+1], data$COHORT_END_DATE[i+1])) {
      # Merge the date ranges
      data$COHORT_START_DATE[i] <- min(data$COHORT_START_DATE[i], data$COHORT_START_DATE[i+1])
      data$COHORT_END_DATE[i] <- max(data$COHORT_END_DATE[i], data$COHORT_END_DATE[i+1])
      # Remove the next row since it's been merged
      data <- data[-(i+1), ]
    } else {
      # Move to the next row if no overlap
      i <- i + 1
    }
  }
  return(data)
}
