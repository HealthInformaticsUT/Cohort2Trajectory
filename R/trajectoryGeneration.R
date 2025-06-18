#
#' Generate trajectories using the logic specified in the configuration
#' 
#' @param cdm object created with CDMConnector 
#' @param stateCohortLabels vector of the customized labels of the state cohorts
#' @param stateCohortPriorityOrder vector of the customized labels of the state cohorts in priority order
#' @param stateCohortMandatory vector of the customized labels of the state cohorts which are mandatory in trajectory
#' @param stateCohortAbsorbing vector of the customized labels of the state cohorts which are absorbing
#' @param stateSelectionType the type of state selection ("First" - First occurring, "Overlap" - Max overlap, "Priority" - Priority)
#' @param oocFix the method to use for replacing "OUT OF COHORT" states with more relevant states ("None" -> "OUT OF COHORT"; "Last present state" -> repeat the last one; random str -> used as state)
#' @param trajectoryType The type of the trajectory ("Discrete" - Discrete time, "Continuous" - Continuous time)
#' @param lengthOfStay The length of stay (days) in one state (Effect only in discrete case)
#' @param outOfCohortAllowed boolean whether the patient trajectory can surpass the target cohort's observation-period
#' @param runSavedStudy running a predefined study from studyName/Settings/trajectorySettings.csv
#' @param useCDM The package can also be run without the OMOP CDM
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param pathToStudy path to directory that contains study folder
#' @param batchSize customizable batch size for trajectory generation process
#' @param updateAutomaticallyForMergedStates boolean for automatically updating priority order and allowed transitions after merging
#' @param studyEnv environment created with cohort2TrajectoryConfiguration 
#'
#' @return dataframe with trajectories
#' @export
#' @examples
#' \dontrun{createTrajectories(cdm = cdm, runSavedStudy = F,studyEnv = studyEnv)}
createTrajectories <- function(cdm = NULL,
                               studyEnv = NULL,
                               trajectoryType = studyEnv$trajectoryType,
                               runSavedStudy = studyEnv$runSavedStudy,
                               oocFix = studyEnv$oocFix,
                               outOfCohortAllowed = studyEnv$outOfCohortAllowed,
                               lengthOfStay = studyEnv$lengthOfStay,
                               stateCohortLabels = studyEnv$stateCohortLabels,
                               stateCohortPriorityOrder = studyEnv$stateCohortPriorityOrder,
                               stateSelectionType = studyEnv$stateSelectionType,
                               stateCohortAbsorbing = studyEnv$stateCohortAbsorbing,
                               stateCohortMandatory = studyEnv$stateCohortMandatory,
                               allowedStatesList = studyEnv$allowedStatesList,
                               useCDM = studyEnv$useCDM,
                               pathToStudy = studyEnv$pathToStudy,
                               batchSize = studyEnv$batchSize,
                               updateAutomaticallyForMergedStates = FALSE) {
  
  studyName = studyEnv$studyName
  pathToData = paste0(studyEnv$pathToStudy, '/', studyName, '/', 'Data')
  pathToStudy = studyEnv$pathToStudy
  atlasTarget = studyEnv$atlasTargetCohort
  
  data <- utils::read.csv(
    paste0(
      pathToData,
      "/",
      "importedDataCleaned_",
      atlasTarget,
      ".csv"
    )
  )
  
  data$subject_id = as.integer(data$subject_id)
  data$time_in_cohort = as.integer(data$time_in_cohort)
  data$cohort_start_date = as.Date(data$cohort_start_date)
  data$cohort_end_date = as.Date(data$cohort_end_date)
  data <- data %>% dplyr::arrange(subject_id, cohort_start_date, cohort_end_date)
  
  if (updateAutomaticallyForMergedStates){
    updated_results <- updateMergedStates(
      data = data, 
      stateCohortLabels = stateCohortLabels, 
      stateCohortPriorityOrder = stateCohortPriorityOrder, 
      allowedStatesList = allowedStatesList, 
      stateCohortAbsorbing = stateCohortAbsorbing
    )
    stateCohortLabels <- updated_results$stateCohortLabels
    stateCohortPriorityOrder <- updated_results$stateCohortPriorityOrder
    allowedStatesList <- updated_results$allowedStatesList
    stateCohortAbsorbing <- updated_results$stateCohortAbsorbing
  }

  
  # generation paramaters
  unique_subject_ids <- unique(data$subject_id)
  #batch_size <- 1000
  batches <- split(unique_subject_ids, ceiling(seq_along(unique_subject_ids) / batchSize))
  cli::cli_progress_bar("Generating trajectories ...", type = "iterator", total = length(batches))
  
  # Create an empty dataframe to store the combined results
  result <- data.frame()
  
  if (nrow(dplyr::filter(data, .data$cohort_definition_id != 0)) == 0) {
    cli::cli_abort("{.error No trajectories generated as cohorts' do not increment any trajectory worthy data!}")
    return(NULL)
  }
  
  
  # Creating trajectories
  if (as.numeric(trajectoryType) == 0) {
    i = 0
    for (batch in batches) {
      i = i + 1  #index of batch
      cli::cli_alert_info(paste(paste("Creating batch ", i, "!!!", sep = "")))
      cli::cli_progress_update()
      # Filter the data based on the current batch of subject_id values
      batch_data <- subset(data, data$subject_id %in% batch)
      
      # Call your function with the filtered data
      result <- rbind(
        result,
        getTrajectoriesDiscrete(
          cohortData = batch_data,
          cdm = cdm,
          stateDuration = lengthOfStay,
          oocFix = oocFix,
          stateSelection = stateSelectionType,
          statePriorityVector = stateCohortPriorityOrder,
          absorbingStates = stateCohortAbsorbing,
          studyName = studyName,
          addPersonalData = useCDM,
          allowedStatesList = allowedStatesList
        )
      )
      
      if (nrow(result) == 0) {
        cli::cli_abort("{.error No trajectories generated as cohorts' do not increment any trajectory worthy data!}")
        return(NULL)
      }
      save_object(result,
                  path = paste(pathToData, "/patientDataDiscrete.csv", sep = ""))
      
    }
    cli::cli_alert_info(paste(
      "Saved trajectory dataframe: ",
      pathToData,
      "/patientDataDiscrete.csv",
      sep = ""
    ))
    
  }
  else if (as.numeric(trajectoryType) == 1) {
    i = 0
    for (batch in batches) {
      i = i + 1
      cli::cli_alert_info(paste(paste("Creating batch ", i, "!!!", sep = "")))
      cli::cli_progress_update()
      # Filter the data based on the current batch of subject_id values
      batch_data <- subset(data, data$subject_id %in% batch)
      # Call your function with the filtered data
      result <- rbind(
        result,
        getTrajectoriesContinuous(
          cdm = cdm,
          patientData =  batch_data,
          stateSelection = stateSelectionType,
          statePriorityVector = stateCohortPriorityOrder,
          absorbingStates = stateCohortAbsorbing,
          studyName = studyName,
          addPersonalData = useCDM,
          allowedStatesList = allowedStatesList
        )
      )
      
      if (nrow(result) == 0) {
        cli::cli_abort("{.error No trajectories generated as cohorts' do not increment any trajectory worthy data!}")
        return(NULL)
      }
      
      save_object(result,
                  path = paste0(pathToData, '/', "patientDataContinuous.csv"))
    }

    cli::cli_alert_info(paste(
      "Saved trajectory dataframe: ",
      paste(pathToData, '/', "patientDataContinuous.csv", sep = ""),
      sep = ""
    ))
  }

  cli::cli_progress_done()
  
  # Saving trajectories
  if (useCDM) {
    cli::cli_alert_info("Saving trajectories to the specified temp schema ...")
    
    cdm <- omopgenerics::insertTable(cdm, "test", result, temporary = T)
    cli::cli_alert_info("Trajectories saved to the specified temp schema!")

  }
  
  ############################################################################
  #
  # Saving study settings as new row
  #
  ############################################################################
  
  # TODO function: saveStudySettings?
  if (!runSavedStudy) {
    if (trajectoryType == 0) {
      savedTrajectoryType = "Discrete"
    }
    else {
      savedTrajectoryType = "Continuous"
    }
    
    savedTrajectoryStates <-
      as.vector(sanitize_filenames(stateCohortLabels))
    
    savedPriorityOrder <-
      as.vector(sanitize_filenames(stateCohortPriorityOrder))
    
    savedStateSelectionType <-
      as.vector(sanitize_filenames(stateSelectionType))
    
    savedAbsorbingStates <-
      as.vector(sanitize_filenames(stateCohortAbsorbing))
    
    savedMandatoryStates <-
      as.vector(sanitize_filenames(stateCohortMandatory))
    
    savedLengthOfStay <- lengthOfStay
    savedOutOfCohortAllowed <- as.logical(outOfCohortAllowed)
    savedOutOfCohortFix <- oocFix
    
    # defining a row
newSettings <- data.frame(
  studyName = if (length(studyName) < 1) "Unnamed study" else studyName,
  savedTrajectoryType = if (length(savedTrajectoryType) < 1) "Discrete" else savedTrajectoryType,
  savedTrajectoryStates = if (length(savedTrajectoryStates) < 1) NULL else paste(savedTrajectoryStates, collapse = ","),
  savedPriorityOrder = if (length(savedPriorityOrder) < 1) NULL else paste(savedPriorityOrder, collapse = ","),
  savedStateSelectionType = if (length(savedStateSelectionType) < 1) "First" else as.integer(savedStateSelectionType),
  savedAbsorbingStates = if (length(savedAbsorbingStates) < 1) NULL else paste(savedAbsorbingStates, collapse = ","),
  savedMandatoryStates = if (length(savedMandatoryStates) < 1) NULL else paste(savedMandatoryStates, collapse = ","),
  savedLengthOfStay = if (length(savedLengthOfStay) < 1) 30 else savedLengthOfStay,
  savedOutOfCohortAllowed = if (length(savedOutOfCohortAllowed) < 1) FALSE else savedOutOfCohortAllowed,
  savedOutOfCohortFix = if (length(savedOutOfCohortFix) < 1) "None" else savedOutOfCohortFix
)
    
    settings <-
      utils::read.csv(
        paste(
          pathToStudy,
          "/",
          studyName,
          "/Settings/trajectorySettings.csv",
          sep = ""
        )
      )
    
    if (studyName %in% settings$studyName) {
      studyIndex <- which(settings$studyName == studyName)
      settings[studyIndex, ] <- newSettings
    } else {
      colnames(newSettings) <- colnames(settings)
      settings <- rbind(settings, newSettings)
    }
    
    utils::write.csv(
      settings,
      paste(
        pathToStudy,
        "/",
        studyName,
        "/Settings/trajectorySettings.csv",
        sep = ""
      ),
      row.names = FALSE
    )

    cli::cli_alert_info(paste(
      "Saved settings to: ",
      paste(
        pathToStudy,
        "/",
        studyName,
        "/Settings/trajectorySettings.csv",
        sep = ""
      ),
      sep = ""
    ))
  }
  
  cli::cli_alert_success("Trajectories generated!")
  
  return(result)
}

#' Update Cohort State Information for Merged States
#'
#' Updates stateCohortLabels, stateCohortPriorityOrder, allowedStatesList, and stateCohortAbsorbing
#' when new merged states are introduced.
#'
#' @param data A dataframe containing cohort definitions.
#' @param stateCohortLabels A vector of existing state labels.
#' @param stateCohortPriorityOrder A vector defining the priority order of states.
#' @param allowedStatesList A named list of allowed state transitions.
#' @param stateCohortAbsorbing A vector of absorbing states that prevent further transitions.
#' @param updateAutomaticallyForMergedStates A boolean flag to enable automatic updates.
#' 
#' @return A list containing updated stateCohortLabels, stateCohortPriorityOrder, allowedStatesList, and stateCohortAbsorbing.
#' @keywords internal
updateMergedStates <- function(data, 
                               stateCohortLabels, 
                               stateCohortPriorityOrder, 
                               allowedStatesList, 
                               stateCohortAbsorbing) {
  
    # Identify new merged states
    newStates <- setdiff(unique(data$cohort_definition_id), c("0", stateCohortLabels))
    priority_map <- stats::setNames(seq_along(stateCohortPriorityOrder), stateCohortPriorityOrder)
    
    for (newState in newStates) {
      # Update stateCohortLabels
      stateCohortLabels <- c(stateCohortLabels, newState)
      
      # Split newState into its components
      components <- unlist(strsplit(newState, "\\+"))
      
      # Filter components that exist in priority_map
      valid_components <- components[components %in% names(priority_map)]
      
      if (length(valid_components) > 0) {
        # Get the highest priority index of any component
        insert_position <- min(priority_map[valid_components])
        
        # Insert new state at the correct priority position
        stateCohortPriorityOrder <- append(stateCohortPriorityOrder, newState, after = insert_position - 1)
        
        # Update priority map with the new state
        priority_map <- stats::setNames(seq_along(stateCohortPriorityOrder), stateCohortPriorityOrder)
      }
      
      # Step 1: Add newState as a transition for all its components
      if (!newState %in% names(allowedStatesList)) {
        allowedStatesList[[newState]] <- unique(unlist(allowedStatesList[valid_components]))
      }
      
      # Step 2: Update all states that previously had any of the components as allowed transitions
      for (component in valid_components) {
        # Add the new combination to the individual component's transition list
        allowedStatesList[[component]] <- unique(c(allowedStatesList[[component]], newState))
        
        # Now go through all other states and update their transition lists
        for (state in names(allowedStatesList)) {
          if (component %in% allowedStatesList[[state]]) {
            allowedStatesList[[state]] <- unique(c(allowedStatesList[[state]], newState))
          }
        }
      }
      
      # Step 3: Update stateCohortAbsorbing
      if (any(valid_components %in% stateCohortAbsorbing)) {
        stateCohortAbsorbing <- unique(c(stateCohortAbsorbing, newState))
      }
    }
    
    # Ensure stateCohortPriorityOrder remains unique
    stateCohortPriorityOrder <- unique(stateCohortPriorityOrder)
  
  return(list(
    stateCohortLabels = stateCohortLabels,
    stateCohortPriorityOrder = stateCohortPriorityOrder,
    allowedStatesList = allowedStatesList,
    stateCohortAbsorbing = stateCohortAbsorbing
  ))
}
