################################################################################
#
# Functions which will query relevant data from database and mutate data  frames
# for better usage
#
################################################################################


#' Get all generated cohort IDs
#'
#' This function queries all generated cohorts in Atlas
#'
#' @param connection Connection to database
#' @param dbms Database management system: postgresql, mysql etc.
#' @param resultsSchema Name of the OMOP results schema
#' @return A sorted vector of generated cohorts
#' @keywords internal
getCohorts <- function(connection, dbms, resultsSchema) {
  allCohorts <- DatabaseConnector::querySql(
    connection,
    sql = loadRenderTranslateSql(
      dbms = dbms,
      sql = "select distinct(cohort_definition_id) from @resultsSchema.cohort order by cohort_definition_id",
      resultsSchema = resultsSchema
    )
  )
  allCohorts <- sort(allCohorts$COHORT_DEFINITION_ID)
  return(allCohorts)
}

#' Get selected generated cohorts patient data
#'
#' This function queries all patient ID's in selected cohorts
#'
#' @param connection Connection to database
#' @param dbms Database management system (postgresql, mysql etc.)
#' @param resultsSchema Name of the OMOP results schema
#' @param selectedTarget The sample cohort ID
#' @param selectedStates Vector of included cohort ID's
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
getCohortData <- function(connection,
                          dbms,
                          resultsSchema,
                          cdmSchema,
                          selectedTarget,
                          selectedStates,
                          # customizedStates = NULL,
                          baseUrl = "http://localhost:8080/WebAPI",
                          pathToResults = getwd()) {
  ##############################################################################
  #
  # Save selected cohorts as JSON and SQL
  #
  ##############################################################################
  #ParallelLogger::logInfo("Quering data of selected cohorts from database")
  selectedCohorts <- c(selectedTarget, selectedStates)
  # Delete folder content
  
  do.call(file.remove, list(list.files(
    paste(pathToResults, "/inst/JSON/", sep = ""), full.names = TRUE
  )))
  do.call(file.remove, list(list.files(
    paste(pathToResults, "/inst/SQL/", sep = ""), full.names = TRUE
  )))
  
  if (!is.null(baseUrl)) {
    for (i in 1:length(selectedCohorts)) {
      cohortId = as.integer(selectedCohorts[i])
      object <-
        ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
      json <- .toJSON(object$expression, pretty = TRUE)
      sql <-
        ROhdsiWebApi::getCohortSql(
          baseUrl = baseUrl,
          cohortDefinition = object,
          generateStats = FALSE
        )
      # Target cohort always have "0" value
      if (i == 1) {
        cohortId = 0
      }
      fileConn <-
        file(paste(
          pathToResults,
          "/inst/JSON/",
          as.character(cohortId),
          ".json",
          sep = ""
        ))
      writeLines(json, fileConn)
      close(fileConn)
      fileConn <-
        file(paste(
          pathToResults,
          "/inst/SQL/",
          as.character(cohortId),
          ".sql",
          sep = ""
        ))
      writeLines(sql, fileConn)
      close(fileConn)
      
    }
  }
  
  idString <-
    toString(sprintf("%s", selectedCohorts))
  sql_base <-
    loadRenderTranslateSql(
      dbms = dbms,
      "SELECT * FROM @resultsSchema.cohort WHERE cohort_definition_id in (%s) ORDER BY subject_id, cohort_start_date, cohort_end_date",
      resultsSchema = resultsSchema
    )
  sql_q <- sprintf(sql_base, idString)
  data <- DatabaseConnector::querySql(connection, sql_q)
  
  ##############################################################################
  #
  # We can have a situation where the target cohort is also given
  # as a state cohort
  #
  ##############################################################################
  data_tmp_target <- NULL
  if (selectedTarget %in% selectedStates) {
    data_tmp_target = dplyr::filter(data, COHORT_DEFINITION_ID == selectedTarget)
  }
  
  ##############################################################################
  #
  # For the purpose of easier target cohort handling we should have a special
  # notation for target cohort id, therefore
  #
  ##############################################################################
  
  data <- dplyr::mutate(
    data,
    COHORT_DEFINITION_ID = ifelse(
      COHORT_DEFINITION_ID == selectedTarget,
      0,
      COHORT_DEFINITION_ID
    )
  )
  
  if (!is.null(data_tmp_target)) {
    data = rbind(data, data_tmp_target)
  }
  
  data <- dplyr::select(data,
                        SUBJECT_ID,
                        COHORT_DEFINITION_ID,
                        COHORT_START_DATE,
                        COHORT_END_DATE)
  data$COHORT_DEFINITION_ID = as.character(data$COHORT_DEFINITION_ID)
  return(data)
}

#' This function adds personal data to subjects: age, gender
#'
#' @param cohortData Imported cohort data
#' @param connection Connection to database
#' @param cdmSchema Schema where "person" table resides
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
addPersonalData <- function(cohortData, connection, cdmSchema) {
  data <- cohortData
  ##############################################################################
  #
  # Add state specific person data to each row
  #
  ##############################################################################
  sql_base <-
    loadRenderTranslateSql(
      dbms = dbms,
      "SELECT person_id, gender_concept_id, birth_datetime FROM @cdmSchema.person WHERE person_id in (%s);",
      cdmSchema = cdmSchema
    )
  idString <-
    toString(sprintf("%s", as.numeric(unique(data$SUBJECT_ID))))
  sql_q <- sprintf(sql_base, idString)
  personal_data <- DatabaseConnector::querySql(connection, sql_q)
  colnames(personal_data) <-
    c("SUBJECT_ID", "GENDER_CONCEPT_ID", "BIRTH_DATETIME")
  ##############################################################################
  #
  # Merge two datasets
  #
  ##############################################################################
  data_merged <-
    merge(x = data,
          y = personal_data,
          by = "SUBJECT_ID",
          all.x = TRUE)
  
  ##############################################################################
  #
  # Calculate age when entering state and time in cohort
  #
  ##############################################################################
  data_merged <- dplyr::mutate(data_merged, AGE = round(as.numeric(
    difftime(as.Date(STATE_START_DATE),
             as.Date(BIRTH_DATETIME),
             units = "days") / 365.25
  ), 3))
  data_merged <- dplyr::select(data_merged,!BIRTH_DATETIME)
  return(data_merged)
}
#' This function eliminates patients which do not fulfill the inclusion criteria
#'
#' @param cohortData Imported cohort data
#' @param mandatoryStates States which have to be present in the trajectory, otherwise dropped#'
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
cleanCohortData <- function(cohortData,
                            mandatoryStates,
                            outOfCohortAllowed = FALSE) {
  data_tmp <- cohortData
  ##############################################################################
  #
  # Preserving only patients which are present in the target cohort
  #
  ##############################################################################
  
  # Preserving only patients present in target cohort
  patientsEligible <-
    unique(dplyr::filter(data_tmp, COHORT_DEFINITION_ID == "0")$SUBJECT_ID)
  
  data_tmp <-
    dplyr::filter(data_tmp, SUBJECT_ID %in% patientsEligible)
  ##############################################################################
  #
  # Cleaning data from observations before and after target cohort
  #
  ##############################################################################
  
  # Removing subjects information which have been included before
  # target cohort start date (maybe should just switch the start date
  # then for the same as target cohort)
  
  # Selecting the first occurring target cohort row
  data_target <- dplyr::slice(dplyr::arrange(
    dplyr::group_by(
      dplyr::filter(data_tmp, COHORT_DEFINITION_ID == "0"),
      SUBJECT_ID
    ),
    COHORT_START_DATE
  ), 1L)
    # Selecting information about the states
  data_states <-
    dplyr::filter(data_tmp, COHORT_DEFINITION_ID != "0")
  data_tmp <- rbind(data_target, data_states)
  data_target <-
    dplyr::select(data_target, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE)
  colnames(data_target) <-
    c("SUBJECT_ID", "REFERENCE_START_DATE", "REFERENCE_END_DATE")
  data_tmp <- merge(data_tmp, data_target, by = "SUBJECT_ID")
    # If the state start date is after inclusion criteria end date then let's filter it out
  if (!outOfCohortAllowed) {
    data_tmp <-
      dplyr::filter(data_tmp,!(REFERENCE_END_DATE < COHORT_START_DATE))
  }
  # If the state end date is before inclusion criteria start date then let's filter it out
  data_tmp <-
    dplyr::filter(data_tmp,!(REFERENCE_START_DATE > COHORT_END_DATE))
  
  data_tmp$COHORT_END_DATE <- as.Date(data_tmp$COHORT_END_DATE)
  
  data_tmp$REFERENCE_END_DATE <-
    as.Date(data_tmp$REFERENCE_END_DATE)
  # If the state start date is inside the interval but the end date is outside the interval
  # then cut the endpoint to reference end point
  if (!outOfCohortAllowed) {
    data_tmp <- dplyr::mutate(
      data_tmp,
      COHORT_END_DATE = dplyr::if_else(
        REFERENCE_END_DATE < COHORT_END_DATE,
        as.Date(REFERENCE_END_DATE),
        as.Date(COHORT_END_DATE)
      )
    )
  }
  # If state start date is before inclusion criteria start date then let's change it to the target cohort start date
  data_tmp <- dplyr::mutate(
    data_tmp,
    COHORT_START_DATE =
      dplyr::if_else(
        REFERENCE_START_DATE <= COHORT_START_DATE,
        COHORT_START_DATE,
        REFERENCE_START_DATE
      )
  )
  # Lets prioritize the cohorts giving the target cohort priority 0 others 1
  # We use priority feature to always order target cohort first per patient in the dataframe
  # this positioning is needed for calculating the feature TIME_IN_COHORT
  data_tmp <- dplyr::mutate(data_tmp,
                            PRIORITY = dplyr::if_else(COHORT_DEFINITION_ID == "0", 0, 1))
  data_tmp <-
    dplyr::arrange(data_tmp, SUBJECT_ID, PRIORITY, COHORT_START_DATE)
  
  ##############################################################################
  #
  # Preserving only patients which have the mandatory state(s)
  #
  ##############################################################################
  
  # Mandatory states
  if (!(length(mandatoryStates) == 0 |
        "No mandatory state" %in% mandatoryStates)) {
    for (state in mandatoryStates) {
      tmpPatientsEligible = unique(dplyr::filter(data_tmp, COHORT_DEFINITION_ID == state)$SUBJECT_ID)
      patientsEligible = intersect(patientsEligible, tmpPatientsEligible)
      
    }
  }
  data_tmp <-
    dplyr::filter(data_tmp, SUBJECT_ID %in% patientsEligible)
  
  ##############################################################################
  #
  # Adding feature "TIME IN COHORT"
  #
  ##############################################################################
  
  # Order by patientId, start & end date
  #data_merged = data_merged[order(data_merged[, 1], data_merged[, 3], data_merged[, 4]),]
  
  data_tmp <-
    dplyr::mutate(data_tmp, TIME_IN_COHORT = round(as.numeric(
      difftime(
        as.Date(COHORT_START_DATE),
        as.Date(REFERENCE_START_DATE),
        units = "days"
      ) /
        365.25
    ), 3))
  data_tmp <- dplyr::select(
    data_tmp,
    SUBJECT_ID,
    COHORT_DEFINITION_ID,
    COHORT_START_DATE,
    COHORT_END_DATE,
    TIME_IN_COHORT
  )
  
  return(data_tmp)
}


#' Build table for cohorts which shows the inclusion statistics
#'
#' This function creates an inclusion table from getCohortsPatients() function data
#'
#' @param cohortData Output of getCohortsPatients()
#' @param selectedCohorts Vector of included cohort ID's
#' @param cohortLabels Labels for cohort
#' @return A dataframe with each selected cohort and its relation to the target cohort
#' @keywords internal
getInclusionTable <- function(cohortData,
                              selectedCohorts,
                              cohortLabels = NULL) {
  data <- cohortData
  nr_unique_cohorts <- length(unique(selectedCohorts))
  cohortbyinclusion <-
    as.data.frame(matrix(NA, nrow = nr_unique_cohorts, ncol = 3))
  colnames(cohortbyinclusion) <-
    c('(#) of patients', '(#) in target cohort', "(%) in target cohort")
  rownames(cohortbyinclusion) <- c(unique(selectedCohorts))
  data_inclusion <-
    dplyr::distinct(dplyr::select(data, c(COHORT_DEFINITION_ID, SUBJECT_ID)))
  patients_target <-
    dplyr::select(dplyr::filter(data_inclusion, COHORT_DEFINITION_ID == "0"),
                  c(SUBJECT_ID))
  ##############################################################################
  #
  # It's important that the state cohorts are defined in Atlas so that they are
  # always a subset of the target cohort, this way the statistics won't be time consuming
  #
  ##############################################################################
  for (cohortID in unique(selectedCohorts)) {
    patients_observed <-
      dplyr::select(dplyr::filter(data_inclusion, COHORT_DEFINITION_ID == cohortID),
                    c(SUBJECT_ID))
    nr_of_patients <- nrow(patients_observed)
    nr_in_target <-
      length(intersect(
        patients_target$SUBJECT_ID,
        patients_observed$SUBJECT_ID
      ))
    prc_in_target <-
      ifelse(nr_of_patients == 0, 0.00, round((nr_in_target /
                                                 nr_of_patients) * 100, 2))
    cohortbyinclusion[toString(cohortID),] = c(nr_of_patients, nr_in_target, prc_in_target)
  }
  
  cohortbyinclusion[, 2] <- as.integer(cohortbyinclusion[, 2])
  cohortbyinclusion[, 1] <- as.integer(cohortbyinclusion[, 1])
  if (is.null(cohortLabels) |
      length(cohortLabels) != length(selectedCohorts))
    return(cohortbyinclusion)
  if (cohortLabels[1] == "0") {
    cohortLabels[1] <- "TARGET"
  }
  rownames(cohortbyinclusion) = cohortLabels
  return(cohortbyinclusion)
}

#' Convert the imported cohort data to patient treatmnet trajectories
#'
#' This function outputs a data.frame object which describes the movement of patients between the defined states
#'
#' @param cohortData A data.frame object which is queried with getCohortsPatients function
#' @param stateDuration The length of a discrete state
#' @param pathToResults Path to temp folder for saving objects
#' @param stateSelection The chosen state selection method
#' @param statePriorityVector The order of priority for states
#' @param absorbingStates The absorbing states: patient trajectory ends with it
#' @param addPersonalData Logical, indicating whether or not to add personal data for each patient 
#' @keywords internal
getTrajectoriesDiscrete <- function(connection,
                                    cohortData,
                                    stateDuration = 30,
                                    pathToResults = getwd(),
                                    stateSelection = 1,
                                    statePriorityVector = NULL,
                                    absorbingStates = NULL,
                                    oocFix = "None",
                                    studyName = "",
                                    addPersonalData = TRUE,
                                    allowedStatesList = list()) {
  tmp_data <- dplyr::filter(cohortData, COHORT_DEFINITION_ID != "0")
  tmp_data <-
    dplyr::arrange(tmp_data, SUBJECT_ID, COHORT_START_DATE)
  # Getting all relevant patient ID's
  patientIds <- unique(tmp_data$SUBJECT_ID)

  newPatientData <- getDiscreteStates(
    stateSelection = as.numeric(stateSelection),
    oocFix = oocFix,
    stateDuration = stateDuration,
    patientIDs = patientIds,
    patientData = tmp_data,
    statePriorityVector = statePriorityVector,
    allowedStatesList = allowedStatesList
  )
  ################################################################################
  #
  # Removing absorbing states
  #
  ################################################################################
  newPatientData <- removeAfterAbsorbingStatesDiscrete(
    patientData <- newPatientData,
    patientIDs <- patientIds,
    absorbingStates <- if (is.null(absorbingStates))
      c("No absorbing state")
    else
      absorbingStates
  )
  ##############################################################################
  #
  # Adding personal data
  #
  ##############################################################################
  
  if (addPersonalData) {
  newPatientData <-
    addPersonalData(newPatientData, connection, cdmSchema)
  }
  else {
    newPatientData$GENDER_CONCEPT_ID = 0
    newPatientData$BIRTH_DATETIME = "1900-01-01" 
  }
  ################################################################################
  #
  # Saving data
  #
  ################################################################################
  
  stateSelectionName <- NULL
  if (stateSelection == "1") {
    stateSelectionName <- "FirstOccurring"
  }
  else if (stateSelection == "2") {
    stateSelectionName <- "Overlap"
  }
  else if (stateSelection == "3") {
    stateSelectionName <- "Priority"
  }
  
  states = as.character(c("START", setdiff(
    unique(newPatientData$STATE), c('START', 'EXIT')
  ), "EXIT"))
  n = length(states)
  
  newPatientData$STATE_ID =
    plyr::mapvalues(
      x = newPatientData$STATE,
      from = states,
      to = 1:n,
      warn_missing = FALSE
    )

  save_object(newPatientData,
              path = paste(
                pathToResults,
                paste(
                  "/tmp/datasets/",
                  studyName,
                  "patientData",
                  stateSelectionName,
                  ".csv",
                  sep = ""
                ),
                sep = ""
              ))
  ParallelLogger::logInfo(paste(
    "Saved trajectory dataframe: ",
    pathToResults,
    paste(
      "/tmp/datasets/",
      studyName,
      "patientData",
      stateSelectionName,
      ".csv",
      sep = ""
    ),
    sep = ""
  ))
  
  
  return(newPatientData)
}



#' Get selected profile ID patient data
#'
#' This function queries patient's data from ohdsi_cdm person table
#'
#' @param connection Connection to database
#' @param dbms Database management system (postgresql, mysql etc.)
#' @param cdmSchema Name of the OMOP cdm schema
#' @param personId Person id value in person table
#' @return A dataframe with selected patient's row data from OMOP database
#' @keywords internal
getProfileData <- function(connection,
                           dbms,
                           cdmSchema,
                           personId) {
  ParallelLogger::logInfo("Quering information about the selected person")
  # Error handling
  if (!grepl("\\D", personId)) {
    personData = DatabaseConnector::querySql(
      connection,
      loadRenderTranslateSql(
        dbms = dbms,
        sql = "SELECT birth_datetime, gender_concept_id, race_concept_id FROM @cdmSchema.person WHERE person_id = @personId",
        personId = personId,
        cdmSchema = cdmSchema
      )
    )
    personData$genderString = ifelse(
      personData$GENDER_CONCEPT_ID == 8532,
      "Female",
      ifelse(personData$GENDER_CONCEPT_ID == 8507, "Male", "Unknown")
    )
    personData$birthtimeString = toString(personData$BIRTH_DATETIME)
    return(personData)
  }
  else {
    personData = cbind(c("NA"), c("NA"), c("NA"), c("NA"), c("NA"))
    colnames(personData) = c(
      "BIRTH_DATETIME",
      "GENDER_CONCEPT_ID",
      "RACE_CONCEPT_ID",
      "genderString",
      "birthtimeString"
    )
    return(personData)
  }
}


#' Get chronological Markov state probabilities from raw data
#'
#' This function outputs a data.frame object which describes the chronological movement of patients between the defined states
#'
#' @param cohortData A data.frame object which is queried with getCohortsPatients function
#' @param stateCohorts The IDs of state cohorts
#' @keywords internal
getChronologicalMatrix <- function(cohortData,
                                   stateCohorts) {
  # Let's convert all data to character
  tmp_data <-
    dplyr::mutate(cohortData, across(everything(), as.character))
  # Extract patients' data which is related to the state cohorts
  tmp_data <- dplyr::filter(tmp_data, COHORT_DEFINITION_ID != "0")
  # Order by patientId, start & end date
  tmp_data <-
    as.data.frame(tmp_data[order(tmp_data[, 1], tmp_data[, 3], tmp_data[, 4]),])
  # Getting cohorts' ID's
  states <- as.character(sort(stateCohorts))
  tmp_data <-
    dplyr::arrange(tmp_data, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE)
  M = data.frame(matrix(0, ncol = length(states), nrow = length(states)))
  rownames(M) <- states
  colnames(M) <- states
  for (row in 2:nrow(tmp_data)) {
    # If we have the same subject we continue the movement between states
    if (tmp_data[row, "SUBJECT_ID"] == tmp_data[row - 1, "SUBJECT_ID"]) {
      # We add +1 to the adjacency matrix on the appropriate coordinates
      M[tmp_data[row - 1, "COHORT_DEFINITION_ID"], tmp_data[row, "COHORT_DEFINITION_ID"]] = M[tmp_data[row - 1, "COHORT_DEFINITION_ID"], tmp_data[row, "COHORT_DEFINITION_ID"]] + 1
    }
    # If the subject ID changes we do not make any changes
  }
  # We'll divide each row by it's sum and will get transfer probabilities
  M <- round(prop.table(as.matrix(M), 1), 3)
  # There is a possibility that we have NaN values, therefore we will change them to zeros
  M[is.nan(M)] <- 0
  return(M)
}

################################################################################
#
# Continuous trajectory functions
#
################################################################################

#' Prepare patientData dataframe for msm package
#'
#' This function prepares patientData dataframe for msm package
#'
#' @param connection Connection to the database
#' @param patientData Object of class data.frame with columns SUBJECT_ID, STATE, STATE_START_DATE, STATE_END_DATE
#' @param stateSelection Selection of the type of ordering
#' @param statePriorityVector All states in prioritized order
#' @param absorbingStates Absorbing states in dataframe
#' @param addPersonalData Logical, indicating whether or not to add personal data for each patient 
#' @return A dataframe ready for using msm package
#' @keywords internal
getTrajectoriesContinuous <- function(connection,
                                      patientData,
                                      stateSelection,
                                      statePriorityVector,
                                      absorbingStates = NULL,
                                      studyName = "",
                                      pathToResults = paste(getwd(), "/tmp", sep = ""),
                                      addPersonalData = TRUE,
                                      allowedStatesList = list()) {
  data <- patientData
  data <- dplyr::mutate(
    data,
    COHORT_DEFINITION_ID = ifelse(COHORT_DEFINITION_ID == "0", "START", COHORT_DEFINITION_ID)
  )
  ### Let's find the ending date of last active state
  data_ls <- dplyr::select(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::filter(data, COHORT_DEFINITION_ID != "START"),
        SUBJECT_ID
      ),
      LAST_STATE_DATE = max(as.Date(COHORT_END_DATE))
    ),
    SUBJECT_ID,
    LAST_STATE_DATE
  )
  
  data <- merge(data, data_ls, by = "SUBJECT_ID")
  
  data <- dplyr::mutate(
    data,
    COHORT_END_DATE = dplyr::if_else(
      COHORT_DEFINITION_ID == "START",
      as.Date(COHORT_START_DATE),
      as.Date(COHORT_END_DATE)
    )
  )
  
  # data$COHORT_DEFINITION_ID = as.character(data$COHORT_DEFINITION_ID)
  
  data <- dplyr::arrange(dplyr::bind_rows(
    dplyr::mutate(
      dplyr::summarise(
        dplyr::group_by(data, SUBJECT_ID),
        LAST_STATE_DATE = max(LAST_STATE_DATE),
        TIME_IN_COHORT = as.numeric(difftime(
          LAST_STATE_DATE, min(COHORT_START_DATE)
        )) / 365.25 + (1 / 365.25),
        COHORT_START_DATE = max(as.Date(LAST_STATE_DATE) + 1)
      ),
      COHORT_END_DATE = COHORT_START_DATE,
      COHORT_DEFINITION_ID = "EXIT"
    ),
    data
  ),
  SUBJECT_ID,
  COHORT_START_DATE)
  ##############################################################################
  #
  # When we have prioritized states, we have to make sure that a state with smaller
  # priority won't be used instead of a state with higher priority
  #
  ##############################################################################
  
  # Lets just create a new dataframe and add all the states in the order of priorities
  if (stateSelection == 2) {
    newData <-
      dplyr::filter(data, COHORT_DEFINITION_ID %in% c("START", "EXIT"))
    for (patientID in unique(newData$SUBJECT_ID)) {
      patientData <- dplyr::filter(
        data,
        SUBJECT_ID == patientID &
          COHORT_DEFINITION_ID != "EXIT" &
          COHORT_DEFINITION_ID != "START"
      )
      
      
      for (p in 1:length(statePriorityVector)) {
        priorityData <- dplyr::filter(patientData,
                                      COHORT_DEFINITION_ID == statePriorityVector[p])
        if (nrow(priorityData) == 0) {
          next
        }
        patientData <- dplyr::filter(patientData,
                                     COHORT_DEFINITION_ID != statePriorityVector[p])
        if (nrow(patientData) > 0) {
          for (i in 1:nrow(priorityData)) {
            newpatientData <- patientData[0,]
            for (j in 1:nrow(patientData)) {
              if (priorityData[i,]$COHORT_START_DATE > patientData[j,]$COHORT_START_DATE &
                  priorityData[i,]$COHORT_END_DATE < patientData[j,]$COHORT_END_DATE) {
                if (priorityData[i,]$COHORT_START_DATE > patientData[j,]$COHORT_START_DATE) {
                  head <- patientData[j,]
                  head$COHORT_END_DATE <-
                    priorityData[i,]$COHORT_START_DATE
                  newpatientData <- rbind(newpatientData, head)
                }
                
                if (priorityData[i,]$COHORT_END_DATE < patientData[j,]$COHORT_END_DATE) {
                  tail <- patientData[j,]
                  tail$COHORT_START_DATE <-
                    priorityData[i,]$COHORT_END_DATE
                  newpatientData <- rbind(newpatientData, tail)
                }
              }
              else if (priorityData[i,]$COHORT_START_DATE < patientData[j,]$COHORT_END_DATE &
                       priorityData[i,]$COHORT_END_DATE > patientData[j,]$COHORT_END_DATE) {
                head <- patientData[j,]
                head$COHORT_END_DATE <-
                  priorityData[i,]$COHORT_START_DATE
                newpatientData <- rbind(newpatientData, head)
              }
              else if (priorityData[i,]$COHORT_START_DATE < patientData[j,]$COHORT_START_DATE &
                       priorityData[i,]$COHORT_END_DATE > patientData[j,]$COHORT_START_DATE) {
                tail <- patientData[j,]
                tail$COHORT_START_DATE <-
                  priorityData[i,]$COHORT_END_DATE
                newpatientData <- rbind(newpatientData, tail)
              }
              else {
                newpatientData <- rbind(newpatientData, patientData[j,])
              }
              
            }
            patientData <- newpatientData
          }
        }
        
        newData <- rbind(newData, priorityData)
        
      }
      
      
    }
    # Inherit the new value
    data <- newData
  }
  
  # We should make sure that the time_in_cohort column has differing values for each state for the same patient
  # for developement case, let's just create an artificial difference of 1 day for each colliding date
  data <- dplyr::arrange(data, SUBJECT_ID, TIME_IN_COHORT)
  paient_id <- NA
  last_patient_id <- data$SUBJECT_ID[1]
  last_observed_ts <- data$TIME_IN_COHORT[1]
  coef <- 1
  impact <- 1 / 365.25
  
  data$TIME_IN_COHORT <- round(data$TIME_IN_COHORT, 6)
  
  for (row in 2:nrow(data)) {
    patient_id <- data$SUBJECT_ID[row]
    if (patient_id != last_patient_id |
        last_observed_ts < data$TIME_IN_COHORT[row]) {
      last_patient_id <- patient_id
      last_observed_ts <- data$TIME_IN_COHORT[row]
      coef <- 1
    }
    else {
      data$TIME_IN_COHORT[row] <- data$TIME_IN_COHORT[row] + impact * coef
      last_patient_id <- patient_id
      coef <- coef + 1
      last_observed_ts <- data$TIME_IN_COHORT[row]
    }
  }
  # We have to map states to 1,..., n.
  states <- as.character(c("START", setdiff(
    unique(data$COHORT_DEFINITION_ID), c('START', 'EXIT')
  ), "EXIT"))
  n <- length(states)
  data$STATE <-
    plyr::mapvalues(
      x = data$COHORT_DEFINITION_ID,
      from = states,
      to = 1:n,
      warn_missing = FALSE
    )
  
  data <- dplyr::mutate(data, STATE = as.numeric(STATE))
  data <- dplyr::arrange(data, SUBJECT_ID, TIME_IN_COHORT, STATE)
  data <- dplyr::select(
    data,
    SUBJECT_ID,
    COHORT_DEFINITION_ID,
    COHORT_START_DATE,
    COHORT_END_DATE,
    STATE,
    TIME_IN_COHORT
  )
  
  
  colnames(data) <- c(
    "SUBJECT_ID",
    "STATE",
    "STATE_START_DATE",
    "STATE_END_DATE",
    "STATE_ID",
    "TIME_IN_COHORT"
  )
  
  ################################################################################
  #
  # Removing absorbing states
  #
  ################################################################################
  data <- removeAfterAbsorbingStatesContinuous(
    patientData = data,
    patientIDs = unique(data$SUBJECT_ID),
    absorbingStates = if (is.null(absorbingStates)) {
      c("No absorbing state")
    }
    else {
      absorbingStates
    }
  )
  
  ##############################################################################
  #
  # Remove prohibited transitions
  #
  ##############################################################################
  
  data <- removeProhibitedTransitionsContinuous(
    patientData = data,
    patientIDs = unique(data$SUBJECT_ID),
    allowedStatesList = allowedStatesList
  )
  
  ##############################################################################
  #
  # Adding personal data
  #
  ##############################################################################
  if (addPersonalData) {
  data <- addPersonalData(data, connection, cdmSchema)
  }
  else {
    data$GENDER_CONCEPT_ID = 0
    data$BIRTH_DATETIME = "1900-01-01" 
  }
  ################################################################################
  #
  # Saving data
  #
  ################################################################################
  
  if (stateSelection == 2) {
    save_object(data,
                path = paste(
                  pathToResults,
                  paste(
                    "/tmp/datasets/",
                    studyName,
                    "patientData",
                    "Continuous",
                    "Priority",
                    ".csv",
                    sep = ""
                  ),
                  sep = ""
                ))
    ParallelLogger::logInfo(paste(
      "Saved trajectory dataframe: ",
      pathToResults,
      paste(
        "/tmp/datasets/",
        studyName,
        "patientData",
        "Continuous",
        "Priority",
        ".csv",
        sep = ""
      ),
      sep = ""
    ))
  }
  else {
    save_object(data,
                path = paste(
                  pathToResults,
                  paste(
                    "/tmp/datasets/",
                    studyName,
                    "patientData",
                    "Continuous",
                    ".csv",
                    sep = ""
                  ),
                  sep = ""
                ))
    ParallelLogger::logInfo(paste(
      "Saved trajectory dataframe: ",
      pathToResults,
      paste(
        "/tmp/datasets/",
        studyName,
        "patientData",
        "Continuous",
        ".csv",
        sep = ""
      ),
      sep = ""
    ))
  }
  return(data)
}


#' Load settings of the study from trajectorySettings.csv according to the customized paramater studyName
#'
#' @param studyName Customized name for the study
#' @keywords internal
loadSettings <- function(studyName) {
  env <-
    rlang::new_environment(data = list(), parent = rlang::empty_env())
  
  jsonFiles = list.files(
    path = paste(pathToResults, "/inst/JSON/", sep = ""),
    pattern = NULL,
    all.files = FALSE,
    full.names = TRUE
  )
  
  stateNamesJSON <- list.files(
    path = paste(pathToResults, "/inst/JSON/", sep = ""),
    pattern = NULL,
    all.files = FALSE,
    full.names = FALSE
  )
  stateNamesJSON <-
    substr(stateNamesJSON, 1, nchar(stateNamesJSON) - 5)
  targetIndex <- which(stateNamesJSON == "0")
  env$stateNamesJSON <- stateNamesJSON[-targetIndex]
  
  env$targetJSON <-
    if (identical(jsonFiles[targetIndex], character(0))) {
      ""
    }
  else {
    paste(readLines(jsonFiles[targetIndex]), collapse = "\n")
  }
  env$jsonFiles <-  jsonFiles[-targetIndex]
  
  insertedJSONs <- c()
  
  for (jsonFile in env$jsonFiles) {
    insertedJSONs <-
      c(insertedJSONs, paste(readLines(jsonFile), collapse = "\n"))
  }
  
  env$insertedJSONs <- insertedJSONs
  
  settings <-
    read.csv(paste(
      pathToResults,
      "/inst/Settings/trajectorySettings.csv",
      sep = ""
    ))
  
  savedStudyNames <- settings$studyName
  
  studyIndex <- which(savedStudyNames == studyName)
  
  if (length(studyIndex) == 0) {
    return(NULL)
  }
  
  env$savedTrajectoryType <- settings$trajectoryType[studyIndex]
  env$savedTrajectoryStates <-
    strsplit(settings$trajectoryStates[studyIndex], ",")[[1]]
  env$savedPriorityOrder <-
    strsplit(settings$priorityOrder[studyIndex], ",")[[1]]
  env$savedStateSelectionType <-
    settings$stateSelectionType[studyIndex]
  env$savedAbsorbingStates <- settings$absorbingStates[studyIndex]
  env$savedMandatoryStates <- settings$mandatoryStates[studyIndex]
  env$savedLengthOfStay <- settings$lengthOfStay[studyIndex]
  env$savedOutOfCohortAllowed <- settings$outOfCohortAllowed[studyIndex]
  env$outOfCohortFix <- settings$outOfCohortFix[studyIndex]
  
  return(env)
}
