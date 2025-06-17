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
#' @param cdm common data model reference object
#'
#' @return A sorted vector of generated cohorts
#' @keywords internal
getCohorts <- function(cdm) {
  allCohorts <- cdm$cohort %>% dplyr::distinct(.data$cohort_definition_id) %>% dplyr::pull(1)
  
  allCohorts <- sort(allCohorts)
  return(allCohorts)
}

#' Get selected generated cohorts patient data
#'
#' This function queries all patient ID's in selected cohorts
#'
#' @param cdm common data model reference object
#' @param selectedTarget The sample cohort ID
#' @param baseUrl the base URL for the WebApi instance
#' @param pathToStudy path to folder which contains folder named after studyName
#' @param studyName name chosen for study, is used as a folder name
#' @param selectedStates Vector of included cohort ID's
#' @param studyEnv environment created with cohort2TrajectoryConfiguration  
#' @param stateCohortLabels Vector of the customized labels of the state cohorts
#'
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
getCohortData <- function(cdm,
                          studyEnv,
                          selectedTarget,
                          selectedStates,
                          baseUrl = "http://localhost:8080/WebAPI",
                          pathToStudy = studyEnv$pathToStudy,
                          studyName = studyEnv$studyName,
                          stateCohortLabels = NULL) {
  ##############################################################################
  #
  # Save selected cohorts as JSON
  #
  ##############################################################################
  selectedCohorts <- c(selectedTarget, selectedStates)
  
  # Read json files from atlas

  if (!is.null(baseUrl)) {
    for (i in 1:length(selectedCohorts)) {
      cohortId = as.integer(selectedCohorts[i])
      object <-
        ROhdsiWebApi::getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
      json <- .toJSON(object$expression, pretty = TRUE)
      # Target cohort always has "0" value
      if (i == 1) {
        cohortId = 0
        selectedCohorts[i] = "0"
      }
      fileConn <-
        file(paste(
          pathToStudy,
          "/",
          studyName,
          "/JSON/",
          as.character(cohortId),
          ".json",
          sep = ""
        ))
      writeLines(json, fileConn)
      close(fileConn)
      
    }
  }
  
  data = NULL
  
  cohortSetCSV <- generateCohortSetCSV(selectedCohorts, stateCohortLabels, pathToStudy, studyName)
  
  selectedCohortIds <- cohortSetCSV$cohortId
  selectedCohortNames <- cohortSetCSV$cohortName
  
  cohortSet <- CDMConnector::readCohortSet(paste(pathToStudy, "/", studyName, "/JSON/", sep = ""))
  
  cdm <- CDMConnector::generateCohortSet(cdm, cohortSet, name = "cohort")
  
  data <- cdm$cohort %>%
    dplyr::filter(.data$cohort_definition_id %in% selectedCohortIds) %>%
    dplyr::arrange(.data$subject_id,
                   .data$cohort_start_date,
                   .data$cohort_end_date) %>%
    dplyr::collect()
  
  data$cohort_definition_id = cohortSetCSV$cohortName[match(data$cohort_definition_id, cohortSetCSV$cohortId)]
  ##############################################################################
  #
  # We can have a situation where the target cohort is also given
  # as a state cohort
  #
  ##############################################################################
  data_tmp_target <- NULL
  if (selectedTarget %in% selectedStates) {
    data_tmp_target = dplyr::filter(data, .data$cohort_definition_id == selectedTarget)
  }
  
  ##############################################################################
  #
  # For the purpose of easier target cohort handling we should have a special
  # notation for target cohort id, therefore
  #
  ##############################################################################
  
  data <- dplyr::mutate(
    data,
    cohort_definition_id = ifelse(
      .data$cohort_definition_id == "target",
      0,
      .data$cohort_definition_id
    )
  )
  
  if (!is.null(data_tmp_target)) {
    data = rbind(data, data_tmp_target)
  }
  
  data <- dplyr::select(
    data,
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  )
  
  return(data)
}

#' This function adds personal data to subjects: age, gender
#'
#' @param cdm a cdm object
#' @param cohortData Imported cohort data
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
addPersonalData <- function(cohortData, cdm) {
  data <- cohortData
  ##############################################################################
  #
  # Add state specific person data to each row
  #
  ##############################################################################
  
  subjects <- data %>% dplyr::distinct(.data$subject_id) %>% dplyr::pull(1) #getCohorts(data, useCDM = FALSE) #TODO: The $ method is for internal use only
  
  personal_data <- cdm$person %>%
    dplyr::filter(.data$person_id %in% subjects)
  
  personal_data <- personal_data %>% 
    dplyr::select("person_id", "gender_concept_id", "birth_datetime") %>%
    dplyr::rename(subject_id = "person_id")
  
  
  
  ##############################################################################
  #
  # Merge two datasets
  #
  ##############################################################################
  data_merged <-
    merge(x = data,
          y = personal_data,
          by = "subject_id",
          all.x = TRUE)
  
  ##############################################################################
  #
  # Calculate age when entering state and time in cohort
  #
  ##############################################################################
  data_merged <- dplyr::mutate(data_merged, age = round(as.numeric(
    difftime(
      as.Date(.data$state_start_date),
      as.Date(.data$birth_datetime),
      units = "days"
    ) / 365.25
  ), 3))
  data_merged <- dplyr::select(data_merged, !"birth_datetime")
  return(data_merged)
}

#' This function eliminates patients which do not fulfill the inclusion criteria
#'
#' @param cohortData Imported cohort data
#' @param mandatoryStates States which have to be present in the trajectory, otherwise dropped#'
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param outOfCohortAllowed boolean whether the patient trajectory can surpass the target cohort's observation-period
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE the states will be label-merged given they overlap more than the specified threshold. Can be given as vector, then multiple iterations are runned,
#'
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
cleanCohortData <- function(cohortData,
                            mandatoryStates,
                            outOfCohortAllowed = FALSE,
                            mergeStates = FALSE,
                            mergeThreshold = 0.5) {
  data_tmp <- cohortData
  data_tmp$subject_id = as.integer(data_tmp$subject_id) #convert int64
  
  ##############################################################################
  #
  # Preserving only patients which are present in the target cohort
  #
  ##############################################################################
  
  # Preserving only patients present in target cohort
  patientsEligible <- data_tmp %>%
    dplyr::filter(.data$cohort_definition_id == "0") %>%
    dplyr::pull(.data$subject_id) %>%
    unique()
  
  data_tmp <-
    dplyr::filter(data_tmp, .data$subject_id %in% patientsEligible)
  ##############################################################################
  #
  # Cleaning data from observations before and after target cohort
  #
  ##############################################################################
  # Removing subjects information which have been included before
  # target cohort start date (maybe should just switch the start date
  # then for the same as target cohort)
  
  # Selecting the first occurring target cohort row
  data_target <- data_tmp %>%
    dplyr::filter(.data$cohort_definition_id == "0") %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::arrange(.data$cohort_start_date, .by_group = TRUE) %>%
    dplyr::slice(1L)
  
  # Selecting information about the states
  data_states <-
    dplyr::filter(data_tmp, .data$cohort_definition_id != "0")
  
  # Merging all cases where patient has same state overlaps' can occur with custom datasets not cohorts
  data_states <- data_states %>%
    dplyr::group_by(.data$subject_id, .data$cohort_definition_id) %>%
    dplyr::do(merge_overlaps(.)) %>%
    dplyr::arrange(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date)
  
  
  # If mergeStates true
  if (mergeStates) {
    cli::cli_progress_step("Merging labels according to the specified threshold!")
    data_states <-
      combineCohorts(data_states,
                     mergeThreshold,
                     unique(data_states$subject_id))
    data_states <-data_states %>% dplyr::arrange(.data$cohort_start_date, .data$cohort_end_date)
    cli::cli_alert_success("Label merging completed!")
  }
  
  data_tmp <- rbind(data_target, data_states)
  data_target <-
    dplyr::select(data_target, "subject_id", "cohort_start_date", "cohort_end_date")
  colnames(data_target) <-
    c("subject_id", "reference_start_date", "reference_end_date")
  data_tmp <- merge(data_tmp, data_target, by = "subject_id")
  # If the state start date is after inclusion criteria end date then let's filter it out
  if (!outOfCohortAllowed) {
    data_tmp <-
      dplyr::filter(data_tmp,
                    !(.data$reference_end_date < .data$cohort_start_date))
  }
  # If the state end date is before inclusion criteria start date then let's filter it out
  data_tmp <-
    dplyr::filter(data_tmp,
                  !(.data$reference_start_date > .data$cohort_end_date))
  
  data_tmp$cohort_end_date <- as.Date(data_tmp$cohort_end_date)
  
  data_tmp$reference_end_date <-
    as.Date(data_tmp$reference_end_date)
  # If the state start date is inside the interval but the end date is outside the interval
  # then cut the endpoint to reference end point
  if (!outOfCohortAllowed) {
    data_tmp <- data_tmp %>%
      dplyr::mutate(
        cohort_end_date = dplyr::if_else(
          .data$reference_end_date < .data$cohort_end_date,
          as.Date(.data$reference_end_date),
          as.Date(.data$cohort_end_date)
        )
      )
    
  }
  # If state start date is before inclusion criteria start date then let's change it to the target cohort start date
  data_tmp <- dplyr::mutate(
    data_tmp,
    cohort_start_date =
      dplyr::if_else(
        .data$reference_start_date <= .data$cohort_start_date,
        .data$cohort_start_date,
        .data$reference_start_date
      )
  )
  # Lets prioritize the cohorts giving the target cohort priority 0 others 1
  # We use priority feature to always order target cohort first per patient in the dataframe
  # this positioning is needed for calculating the feature time_in_cohort
  data_tmp <- dplyr::mutate(data_tmp,
                            PRIORITY = dplyr::if_else(.data$cohort_definition_id == "0", 0, 1))
  data_tmp <-
    dplyr::arrange(data_tmp,
                   .data$subject_id,
                   .data$PRIORITY,
                   .data$cohort_start_date)
  
  ##############################################################################
  #
  # Preserving only patients which have the mandatory state(s)
  #
  ##############################################################################
  
  # Mandatory states
  if (!(length(mandatoryStates) == 0 |
        "No mandatory state" %in% mandatoryStates)) {
    for (state in mandatoryStates) {
      tmpPatientsEligible = unique(dplyr::filter(data_tmp, .data$cohort_definition_id == state)$subject_id)
      patientsEligible = intersect(patientsEligible, tmpPatientsEligible)
      
    }
  }
  
  data_tmp <-
    dplyr::filter(data_tmp, .data$subject_id %in% patientsEligible)
  ##############################################################################
  #
  # Adding feature "TIME IN COHORT"
  #
  ##############################################################################
  
  # Order by patientId, start & end date
  #data_merged = data_merged[order(data_merged[, 1], data_merged[, 3], data_merged[, 4]),]
  data_tmp <-
    dplyr::mutate(data_tmp, time_in_cohort = round(as.numeric(
      difftime(
        as.Date(.data$cohort_start_date),
        as.Date(.data$reference_start_date),
        units = "days"
      ) /
        365.25
    ), 3))
  
  data_tmp <- dplyr::select(
    data_tmp,
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date",
    "time_in_cohort"
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
#'
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
  
  data_inclusion <- data %>%
    dplyr::select(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::distinct()
  
  patients_target <- data_inclusion %>%
    dplyr::filter(.data$cohort_definition_id == "0") %>%
    dplyr::select("subject_id")
  
  ##############################################################################
  #
  # It's important that the state cohorts are defined in Atlas so that they are
  # always a subset of the target cohort, this way the statistics won't be time consuming
  #
  ##############################################################################
  for (cohortID in unique(selectedCohorts)) {
    patients_observed <- data_inclusion %>%
      dplyr::filter(.data$cohort_definition_id == cohortID) %>%
      dplyr::select("subject_id")
    
    nr_of_patients <- nrow(patients_observed)
    nr_in_target <-
      length(intersect(
        patients_target$subject_id,
        patients_observed$subject_id
      ))
    prc_in_target <-
      ifelse(nr_of_patients == 0, 0.00, round((nr_in_target /
                                                 nr_of_patients) * 100, 2))
    cohortbyinclusion[toString(cohortID), ] = c(nr_of_patients, nr_in_target, prc_in_target)
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
#' @param stateSelection The chosen state selection method
#' @param statePriorityVector The order of priority for states
#' @param absorbingStates The absorbing states: patient trajectory ends with it
#' @param cdm a cdm object
#' @param oocFix The method to use for replacing "OUT OF COHORT" states with more relevant states
#' @param studyName name chosen for study, is used as a folder name
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param addPersonalData Logical, indicating whether or not to add personal data for each patient
#' @keywords internal
getTrajectoriesDiscrete <- function(cdm,
                                    cohortData,
                                    stateDuration = 30,
                                    stateSelection = 1,
                                    statePriorityVector = NULL,
                                    absorbingStates = NULL,
                                    oocFix = "None",
                                    studyName = "",
                                    addPersonalData = TRUE,
                                    allowedStatesList = list()) {
  tmp_data <- dplyr::filter(cohortData, .data$cohort_definition_id != "0")
  tmp_data <-
    dplyr::arrange(tmp_data, "subject_id", "cohort_start_date")
  
  # Getting all relevant patient ID's
  patientIds <- unique(tmp_data$subject_id)
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
      addPersonalData(newPatientData, cdm)
  }
  else {
    newPatientData$gender_concept_id = 0
    newPatientData$birth_datetime = "1900-01-01"
  }
  ################################################################################
  #
  # Saving data
  #
  ################################################################################
  
  states = as.character(c("START", setdiff(
    unique(newPatientData$state_label), c('START', 'EXIT')
  ), "EXIT"))
  n = length(states)
  
  newPatientData$state_id =
    plyr::mapvalues(
      x = newPatientData$state_label,
      from = states,
      to = 1:n,
      warn_missing = FALSE
    )
  
  return(newPatientData)
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
#' @param patientData Object of class data.frame with columns subject_id, state_label, STATE_START_DATE, STATE_END_DATE
#' @param stateSelection Selection of the type of ordering
#' @param statePriorityVector All states in prioritized order
#' @param absorbingStates Absorbing states in dataframe
#' @param cdm a cdm object
#' @param studyName name chosen for study, is used as a folder name
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param addPersonalData Logical, indicating whether or not to add personal data for each patient
#' @return A dataframe ready for using msm package
#' @keywords internal
getTrajectoriesContinuous <- function(cdm,
                                      patientData,
                                      stateSelection,
                                      statePriorityVector,
                                      absorbingStates = NULL,
                                      studyName = "",
                                      addPersonalData = TRUE,
                                      allowedStatesList = list()) {
  data <- patientData
  data <- dplyr::mutate(
    data,
    cohort_definition_id = ifelse(
      .data$cohort_definition_id == "0",
      "START",
      .data$cohort_definition_id
    )
  )
  
  ### Let's find the ending date of last active state
  data_ls <- data %>%
    dplyr::filter(.data$cohort_definition_id != "START") %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::summarise(LAST_STATE_DATE = max(as.Date(.data$cohort_end_date), na.rm = TRUE)) %>%
    dplyr::select("subject_id", "LAST_STATE_DATE")
  
  data <- merge(data, data_ls, by = "subject_id")
  
  data <- dplyr::mutate(
    data,
    cohort_end_date = dplyr::if_else(
      .data$cohort_definition_id == "START",
      as.Date(.data$cohort_start_date),
      as.Date(.data$cohort_end_date)
    )
  )
  
  # data$cohort_definition_id = as.character(data$cohort_definition_id)
  # Adding "EXIT" state
  data <- data %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::summarise(
      LAST_STATE_DATE = max(.data$LAST_STATE_DATE, na.rm = TRUE),
      cohort_start_date = max(as.Date(.data$LAST_STATE_DATE) + 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = "EXIT"
    ) %>%
    dplyr::bind_rows(data) %>%
    dplyr::arrange(.data$subject_id, .data$cohort_start_date)
  
  
  
  ##############################################################################
  #
  # When we have prioritized states, we have to make sure that a state with smaller
  # priority won't be used instead of a state with higher priority
  #
  ##############################################################################
  
  # Lets just create a new dataframe and add all the states in the order of priorities
  if (stateSelection == 3) {
    newData <-
      dplyr::filter(data, .data$cohort_definition_id %in% c("START", "EXIT"))
    for (patientID in unique(newData$subject_id)) {
      patientData <- dplyr::filter(
        data,
        .data$subject_id == patientID &
        .data$cohort_definition_id != "EXIT" &
        .data$cohort_definition_id != "START"
      )
      
      
      for (p in 1:length(statePriorityVector)) {
        priorityData <- dplyr::filter(patientData,
                                      .data$cohort_definition_id == statePriorityVector[p])
        if (nrow(priorityData) == 0) {
          next
        }
        patientData <- dplyr::filter(patientData,
                                     .data$cohort_definition_id != statePriorityVector[p])
        if (nrow(patientData) > 0) {
          for (i in 1:nrow(priorityData)) {
            newpatientData <- patientData[0, ]
            for (j in 1:nrow(patientData)) {
              if (nrow(patientData) == 0) {
                break
              }
              if (priorityData[i, ]$cohort_start_date <= patientData[j, ]$cohort_start_date &
                  priorityData[i, ]$cohort_end_date >= patientData[j, ]$cohort_end_date) {
                # In this case the smaller priority state is absorbed by a higher priority state
                next
              }
              else if (priorityData[i, ]$cohort_start_date >= patientData[j, ]$cohort_start_date &
                       priorityData[i, ]$cohort_end_date <= patientData[j, ]$cohort_end_date) {
                if (priorityData[i, ]$cohort_start_date > patientData[j, ]$cohort_start_date) {
                  head <- patientData[j, ]
                  head$cohort_end_date <-
                    priorityData[i, ]$cohort_start_date
                  newpatientData <- rbind(newpatientData, head)
                }
                if (priorityData[i, ]$cohort_end_date <= patientData[j, ]$cohort_end_date) {
                  tail <- patientData[j, ]
                  tail$cohort_start_date <-
                    priorityData[i, ]$cohort_end_date + 1
                  newpatientData <- rbind(newpatientData, tail)
                }
              }
              else if (priorityData[i, ]$cohort_start_date <= patientData[j, ]$cohort_end_date &
                       priorityData[i, ]$cohort_end_date >= patientData[j, ]$cohort_end_date) {
                head <- patientData[j, ]
                head$cohort_end_date <-
                  priorityData[i, ]$cohort_start_date
                newpatientData <- rbind(newpatientData, head)
                next
              }
              else if (priorityData[i, ]$cohort_start_date <= patientData[j, ]$cohort_start_date &
                       priorityData[i, ]$cohort_end_date >= patientData[j, ]$cohort_start_date) {
                tail <- patientData[j, ]
                tail$cohort_start_date <-
                  priorityData[i, ]$cohort_end_date
                newpatientData <- rbind(newpatientData, tail)
              }
              else {
                newpatientData <- rbind(newpatientData, patientData[j, ])
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
  
  # Calculating other time_in_cohort values
  data_target <- dplyr::slice(dplyr::arrange(
    dplyr::group_by(
      dplyr::filter(data, .data$cohort_definition_id == "START"),
      .data$subject_id
    ),
    .data$cohort_start_date
  ),
  1L)
  # Selecting information about the states
  data_states <-
    dplyr::filter(data, .data$cohort_definition_id != "START")
  data <- rbind(data_target, data_states)
  data_target <-
    dplyr::select(data_target, "subject_id", "cohort_start_date", "cohort_end_date")
  colnames(data_target) <-
    c("subject_id", "reference_start_date", "reference_end_date")
  data <- merge(data, data_target, by = "subject_id")
  data <- dplyr::mutate(data, time_in_cohort = round(as.numeric(
    difftime(
      as.Date(.data$cohort_start_date),
      as.Date(.data$reference_start_date),
      units = "days"
    ) /
      365.25
  ), 3))
  data <- dplyr::select(
    data,
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date",
    "time_in_cohort"
  )
  
  data <- dplyr::arrange(data, .data$subject_id, .data$time_in_cohort)
  # We have to map states to 1,..., n.
  states <- as.character(c("START", setdiff(
    unique(data$cohort_definition_id), c('START', 'EXIT')
  ), "EXIT"))
  n <- length(states)
  data$state_label <-
    plyr::mapvalues(
      x = data$cohort_definition_id,
      from = states,
      to = 1:n,
      warn_missing = FALSE
    )
  data <- dplyr::mutate(data, state_label = as.numeric(.data$state_label))
  data <- dplyr::arrange(data, .data$subject_id, .data$time_in_cohort, .data$state_label)
  
  data <- dplyr::select(
    data,
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date",
    "state_label",
    "time_in_cohort"
  )
  
  
  colnames(data) <- c(
    "subject_id",
    "state_label",
    "state_start_date",
    "state_end_date",
    "state_id",
    "time_in_cohort"
  )
  
  # We should make sure that the time_in_cohort column has differing values for each state for the same patient
  # for developement case, let's just create an artificial difference of 1 day for each colliding date
  data <- dplyr::arrange(data, .data$subject_id, .data$time_in_cohort, .data$state_id)
  paient_id <- NA
  last_patient_id <- data$subject_id[1]
  last_observed_ts <- data$time_in_cohort[1]
  coef <- 1
  impact <- 1 / 365.25
  
  data$time_in_cohort <- round(data$time_in_cohort, 6)
  
  for (row in 2:nrow(data)) {
    patient_id <- data$subject_id[row]
    if (patient_id != last_patient_id |
        last_observed_ts < data$time_in_cohort[row]) {
      last_patient_id <- patient_id
      last_observed_ts <- data$time_in_cohort[row]
      coef <- 1
    }
    else {
      data$time_in_cohort[row] <- data$time_in_cohort[row] + impact * coef
      last_patient_id <- patient_id
      coef <- coef + 1
      last_observed_ts <- data$time_in_cohort[row]
    }
  }
  
  # Fix state end dates
  data <- data %>%
    dplyr::arrange(.data$subject_id, .data$time_in_cohort) %>% # Arrange by SUBJECT_ID and TIME_IN_COHORT
    dplyr::group_by(.data$subject_id) %>% # Group by SUBJECT_ID
    dplyr::mutate(
      state_end_date = dplyr::case_when(
        state_label %in% c("START", "EXIT") ~ state_end_date,
        # Keep START and EXIT as is
        TRUE ~ dplyr::lead(state_start_date) # Use next STATE_START_DATE
      )
    ) %>%
    dplyr::ungroup() # Ungroup for safety
  
  ################################################################################
  #
  # Removing absorbing states
  #
  ################################################################################
  data <- removeAfterAbsorbingStatesContinuous(
    patientData = data,
    patientIDs = unique(data$subject_id),
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
    patientIDs = unique(data$subject_id),
    allowedStatesList = allowedStatesList
  )
  
  ##############################################################################
  #
  # Adding personal data
  #
  ##############################################################################
  if (addPersonalData) {
    data <- addPersonalData(data, cdm)
  }
  else {
    data$gender_concept_id = 0
    data$birth_datetime = "1900-01-01"
  }
  
  return(data)
}


#' Load settings of the study from trajectorySettings.csv according to the customized paramater studyName
#'
#' @param studyName Customized name for the study
#' @keywords internal
loadSettings <- function(studyName, pathToStudy) {
  env <-
    rlang::new_environment(data = list(), parent = rlang::empty_env())
  
  jsonFiles = list.files(
    path = paste(pathToStudy, "/", studyName, "/JSON", sep = ""),
    pattern = NULL,
    all.files = FALSE,
    full.names = TRUE
  )
  
  stateNamesJSON <- list.files(
    path = paste(pathToStudy, "/", studyName, "/JSON", sep = ""),
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
    utils::read.csv(paste(
      pathToStudy,
      "/",
      studyName,
      "/Settings/trajectorySettings.csv",
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
  env$savedAbsorbingStates <- strsplit(settings$absorbingStates[studyIndex], ",")[[1]]
  env$savedMandatoryStates <- strsplit(settings$mandatoryStates[studyIndex], ",")[[1]]
  env$savedLengthOfStay <- settings$lengthOfStay[studyIndex]
  env$savedOutOfCohortAllowed <-
    settings$outOfCohortAllowed[studyIndex]
  env$outOfCohortFix <- settings$outOfCohortFix[studyIndex]
  
  return(env)
}

#' Generate cohort set CSV
#'
#' internal function for generating cohort set CSV for CDMConnector generateCohorts
#'
#' @param selectedStates all states to be included in db query
#' @param pathToStudy path to folder that contains study folder
#' @param studyName name of the study directory
#' @param stateCohortLabels user inserted labels for non target states
#'
#' @return cohortSetCSV df 
#' @keywords internal
generateCohortSetCSV <- function(selectedStates,
                                 stateCohortLabels,
                                 pathToStudy,
                                 studyName) {
  if (file.exists(paste0(pathToStudy, studyName, '/JSON/CohortsToCreate.csv'))) {
    file.remove(paste0(pathToStudy, studyName, '/JSON/CohortsToCreate.csv'))
  }
  
  cohortName <- c("target", stateCohortLabels)
  jsonPath <- paste0(selectedStates, ".json")
  
  max <- length(cohortName)
  cohortId <- c(1:max)
  
  cohortSet <- data.frame(cohortId, cohortName, jsonPath)
  
  cohortList <- list(
    cohortId = cohortSet$cohortId,
    cohortName = cohortSet$cohortName,
    jsonPath = cohortSet$jsonPath
  )
  save_object(cohortSet,
              paste0(pathToStudy, "/", studyName, '/JSON/CohortsToCreate.csv'))
  
  return (cohortSet)
}
