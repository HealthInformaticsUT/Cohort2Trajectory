#' Get data for study
#'
#' 
#' @param cdm object created with CDMConnector 
#' @param baseUrl the base URL for the WebApi instance
#' @param atlasTargetCohort the id of the target cohort defined in OHDSI tool ATLAS
#' @param atlasStateCohorts the ids of the state cohorts defined in OHDSI tool ATLAS
#' @param stateCohortLabels vector of the customized labels of the state cohorts
#' @param stateCohortPriorityOrder vector of the customized labels of the state cohorts in priority order
#' @param stateCohortMandatory vector of the customized labels of the state cohorts which are mandatory in trajectory
#' @param outOfCohortAllowed boolean whether the patient trajectory can surpass the target cohort's observation-period
#' @param runSavedStudy running a predefined study from studyName/Settings/trajectorySettings.csv
#' @param useCDM The package can also be run without the OMOP CDM
#' @param trajectoryDataObject When using without OMOP CDM specify the data file  (if specified no need for pathToData)
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE the states will be label-merged given they overlap more than the specified threshold. Can be given as vector, then multiple iterations are runned,
#' @param pathToData When using without OMOP CDM specify the path to data file (if specified no need for trajectoryDataObject)
#' @param studyEnv environment created with cohort2TrajectoryConfiguration 
#'
#' @return log info?
#' @export
#' @examples
#' \dontrun{getDataForStudy(cdm = cdm,studyEnv = studyEnv)}
getDataForStudy <- function(runSavedStudy = studyEnv$runSavedStudy,
                            useCDM = studyEnv$useCDM,
                            trajectoryDataObject = NULL,
                            cdm = NULL,
                            studyEnv = NULL,
                            #settings = NULL,
                            atlasStateCohorts = studyEnv$atlasStateCohorts,
                            atlasTargetCohort = studyEnv$atlasTargetCohort,
                            stateCohortLabels = studyEnv$stateCohortLabels,
                            baseUrl = studyEnv$baseUrl,
                            outOfCohortAllowed = studyEnv$outOfCohortAllowed,
                            stateCohortMandatory = studyEnv$stateCohortMandatory,
                            stateCohortPriorityOrder = studyEnv$stateCohortPriorityOrder,
                            mergeStates = studyEnv$mergeStates,
                            mergeThreshold = studyEnv$mergeThreshold,
                            pathToData = NULL)  {
  
  # Variables from global studyEnv (can't be altered)
  studyName = studyEnv$studyName
  pathToStudy = studyEnv$pathToStudy
  
  
  cli::cli_progress_step("Importing data ...")
  if (runSavedStudy) {
    # get data from cdm using existing json files and previous study settings
    
    # loading settings
    studyName <- sanitize_single(studyName)
    settings <- loadSettings(studyName, pathToStudy)
    
    stateNamesJSON <- c("0", settings$stateNamesJSON)
    
    # TODO overwrite to studyEnv?
    # Load the study parameters
    stateCohortLabels <- as.vector(sanitize_filenames(settings$savedTrajectoryStates))
    stateCohortPriorityOrder <- as.vector(sanitize_filenames(settings$savedPriorityOrder))
    stateCohortMandatory <- as.vector(sanitize_filenames(settings$savedMandatoryStates))
    stateCohortAbsorbing <- as.vector(sanitize_filenames(settings$savedAbsorbingStates))
    stateSelectionType <- settings$savedStateSelectionType
    oocFix <- settings$outOfCohortFix
    
    selectedTarget <- settings$targetJSON
    selectedStates <- settings$insertedJSONs
    trajectoryType <-
    if (settings$savedTrajectoryType == "Discrete") {
        0
      }
    else {
      1
    }
    
    lengthOfStay <- settings$savedLengthOfStay
    outOfCohortAllowed <- settings$savedOutOfCohortAllowed
    
    data <- getCohortData(
      cdm = cdm,
      studyEnv = studyEnv,
      selectedTarget = "0",
      selectedStates = stateCohortLabels,
      baseUrl = NULL,
      stateCohortLabels = stateCohortLabels
    )
    
    # Apply state names and select
    data$cohort_definition_id <- plyr::mapvalues(
      x = data$cohort_definition_id,
      from = 1:length(stateNamesJSON),
      to = stateNamesJSON,
      warn_missing = FALSE
    )
    
    data <- dplyr::select(data,
                          "subject_id",
                          "cohort_definition_id",
                          "cohort_start_date",
                          "cohort_end_date")
    
    
  } else if (useCDM) {
    # get data from cdm using atlas for json file source
        data <- getCohortData(
      cdm = cdm,
      studyEnv = studyEnv,
      selectedTarget = atlasTargetCohort,
      selectedStates = atlasStateCohorts,
      baseUrl = baseUrl,
      pathToStudy = studyEnv$pathToStudy,
      studyName = studyEnv$studyName,
      stateCohortLabels = stateCohortLabels
    )

    # Change state labels
    
    #column_cohort_definition_id <- dplyr::pull(data, cohort_definition_id)
    data$cohort_definition_id <- plyr::mapvalues(
      x = dplyr::pull(data, .data$cohort_definition_id),
      from = c("0", as.character(atlasStateCohorts)),
      to = c("0", stateCohortLabels),
      warn_missing = FALSE
    )
    
    # Rename JSON file
    if (!is.null(baseUrl)) {
      for (i in 1:length(stateCohortLabels)) {
        file.rename(
          paste(
            pathToStudy,
            "/",
            studyName,
            "/JSON/",
            atlasStateCohorts[i],
            ".json",
            sep = ""
          ),
          paste(
            pathToStudy,
            "/",
            studyName,
            "/JSON/",
            stateCohortLabels[i],
            ".json",
            sep = ""
          )
        )
      }
    }

  } else {
    # get data from input
    
    if (is.null(trajectoryDataObject)) {
      
      data = utils::read.csv(pathToData)
    }
    else{
      trajectoryDataObject$cohort_definition_id = sanitize_filenames(trajectoryDataObject$cohort_definition_id)
      data = trajectoryDataObject
    }
    
  }
  
  cli::cli_progress_done()
  
  # Print completion message
  cli::cli_alert_success("Get cohort data success!")
  
  # arrange data
  data <-
    dplyr::arrange(data,
                   .data$subject_id,
                   .data$cohort_start_date,
                   .data$cohort_end_date,
                   .data$cohort_definition_id)
  
  if (length(dplyr::pull(data, .data$cohort_definition_id)) == 0) {
    return(
      cli::cli_warn("{.warning There were no patients imported! Check your target cohort!}")
    )
  }
  
  cli::cli_progress_step("Cleaning data ...")
  
  data <- cleanCohortData(
    cohortData = data,
    mandatoryStates = stateCohortMandatory,
    outOfCohortAllowed = outOfCohortAllowed,
    mergeStates = mergeStates,
    mergeThreshold = mergeThreshold
  )
  
  
  # As we may have new state labels (if mergeStates = TRUE) we now will modify some settings:
  if (mergeStates) {
    stateCohortPriorityOrder <-
      ordered_combinations(stateCohortPriorityOrder, n = length(mergeThreshold) + 1)
    
    allowedStatesList_updated <-
      lapply(names(allowedStatesList), function(state_name) {
        c(allowedStatesList[[state_name]], stateCohortPriorityOrder[grepl(state_name, stateCohortPriorityOrder)])
      })
    
    names(allowedStatesList_updated) <- names(allowedStatesList)
    
    for (state_name in stateCohortPriorityOrder) {
      allowedStatesList_updated[[state_name]] <-
        allowedStatesList_updated[[strsplit(state_name, split = "\\+")[[1]][1]]]
    }
    
    allowedStatesList <- allowedStatesList_updated
    
    stateCohortAbsorbing <-
      unique(unlist(lapply(stateCohortAbsorbing, function(state_name) {
        stateCohortPriorityOrder[grepl(state_name, stateCohortPriorityOrder)]
      })))
  }
  saveObjectId = ifelse(is.null(atlasTargetCohort), "0", atlasTargetCohort)
  cli::cli_alert_success("Data cleaning completed!")
  
  save_object(
    path =  paste(
      pathToStudy,
      "/",
      studyName,
      "/Data/",
      "importedDataCleaned_",
      atlasTargetCohort,
      ".csv",
      sep = ""
    ),
    object = data
  )

  cli::cli_alert_info(paste0(
    "Saved cleaned data ",
    pathToStudy,
    "/",
    studyName,
    "/Data/",
    "importedDataCleaned_",
    atlasTargetCohort,
    ".csv"
  ))
  
  if (length(dplyr::pull(data, .data$cohort_definition_id)) == 0) {
    cli::cli_warn("{.warning No patients left after cleaning the data!}")
    
    return(NULL)
  }
  
  if (length(unique(data$cohort_definition_id)) < 2) {
    return(
      cli::cli_warn("{.warning No state data left after cleaning the imported data! Exiting ...}")
    )
  }
}
