################################################################################
#
# Run the trajectory creator
#
################################################################################

#' This function creates patient treatment trajectories
#'
#' @param connection Connection to database
#' @param dbms The type of DBMS running on the server. Valid values are: 'oracle','postgresql','redshift','sql server','pdw', 'netezza','bigquery','sqlite', 'sqlite extended','spark'
#' @param cdmSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmVocabSchema Schema which contains the OHDSI Common Data Model vocabulary tables.
#' @param cdmTmpSchema Schema for temporary tables, will be deleted.
#' @param cdmResultsSchema Schema which has the information about the cohorts created in Atlas
#' @param atlasTargetCohort The id of the target cohort defined in OHDSI tool ATLAS
#' @param atlasStateCohorts The ids of the state cohorts defined in OHDSI tool ATLAS
#' @param stateCohortLabels Vector of the customized labels of the state cohorts
#' @param stateCohortPriorityOrder Vector of the customized labels of the state cohorts in priority order
#' @param stateCohortMandatory Vector of the customized labels of the state cohorts which are mandatory in trajectory
#' @param stateCohortAbsorbing Vector of the customized labels of the state cohorts which are absorbing
#' @param stateSelectionType The type of state selection (1 - First occurring, 2 - Max overlap, 3 - Priority)
#' @param oocFix The method to use for replacing "OUT OF COHORT" states with more relevant states
#' @param trajectoryType The type of the trajectory (0 - Discrete time, 1 - Continuous time)
#' @param lengthOfStay The length of stay (days) in one state (Effect only in discrete case)
#' @param outOfCohortAllowed boolean whether the patient trajectory can surpass the target cohort's observation-period
#' @param useCDM The package can also be run without the OMOP CDM
#' @param pathToData When using without OMOP CDM specify the path to data file
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE the states will be label-merged given they overlap more than the specified threshold. Can be given as vector, then multiple iterations are runned,
#' @example man/examples/Cohort2Trajectory.R
#'
#' @export
Cohort2Trajectory <- function(dbms = "postgresql",
                              connection = NULL,
                              cdmSchema = "ohdsi_cdm",
                              cdmVocabSchema = "ohdsi_vocab",
                              cdmTmpSchema = "ohdsi_temp",
                              cdmResultsSchema = "ohdsi_results",
                              studyName = "Cohort2Trajectory",
                              baseUrl = "http://localhost:8080/WebAPI",
                              atlasTargetCohort = NULL,
                              atlasStateCohorts = NULL,
                              stateCohortLabels = NULL,
                              stateCohortPriorityOrder = NULL,
                              stateCohortMandatory = NULL,
                              stateCohortAbsorbing = NULL,
                              stateSelectionType = NULL,
                              oocFix = "None",
                              trajectoryType = NULL,
                              lengthOfStay = NULL,
                              outOfCohortAllowed = NULL,
                              runSavedStudy = FALSE,
                              pathToResults = getwd(),
                              useCDM = TRUE,
                              pathToData = './tmp/datasets/importedData.csv',
                              allowedStatesList = createStateList(stateCohortLabels),
                              mergeStates = FALSE,
                              mergeThreshold = 0.5
) {
  ###############################################################################
  #
  # Creating mandatory directories if they do not exist
  #
  ###############################################################################
  
  createMandatorySubDirs(pathToResults)
  
  ##############################################################################
  #
  # Creating global variables
  #
  ##############################################################################
  
   dbms <<- dbms
   conn <<- connection
   cdmSchema <<- cdmSchema
   cdmTmpSchema <<- cdmTmpSchema
   cdmResultsSchema <<- cdmResultsSchema
  
  ##############################################################################
  
  data <- NULL
  
  # running already defined study
  if (runSavedStudy) {
    cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
    # loading settings
    settings <- loadSettings(studyName)
    stateNamesJSON <- c("0", settings$stateNamesJSON)
    insertedJSONs <- c(settings$targetJSON, settings$insertedJSONs)
    
    for (i in 1:length(stateNamesJSON)) {
      cohortJson <- insertedJSONs[i]
      cohortName <- stateNamesJSON[i]
      # creating cohorts
      cohortExpression <-
        CirceR::cohortExpressionFromJson(cohortJson)
      cohortSql <-
        CirceR::buildCohortQuery(cohortExpression,
                                 options = CirceR::createGenerateOptions(generateStats = FALSE))
      cohortsToCreate <-
        rbind(
          cohortsToCreate,
          data.frame(
            cohortId = i,
            cohortName = cohortName,
            sql = cohortSql,
            stringsAsFactors = FALSE
          )
        )
    }
    
    ############################################################################
    #
    # Generate the saved states in database
    #
    ############################################################################
    # Create the cohort tables to hold the cohort generation results
    cohortTableNames <-
      CohortGenerator::getCohortTableNames(cohortTable = studyName)
    CohortGenerator::createCohortTables(
      connection = connection,
      cohortDatabaseSchema = cdmTmpSchema,
      cohortTableNames = cohortTableNames
    )
    # Generate the cohorts
    generateCohortSet(
      connection = connection,
      cdmDatabaseSchema = cdmSchema,
      cdmVocabSchema = cdmVocabSchema,
      cohortDatabaseSchema = cdmTmpSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = cohortsToCreate
    )
    sql <-
      loadRenderTranslateSql(
        dbms = dbms,
        "SELECT * FROM @cdmTmpSchema.@studyName",
        cdmTmpSchema = cdmTmpSchema,
        studyName = studyName
      )
    
    ############################################################################
    #
    # Set the study parameters
    #
    ############################################################################
    
    stateCohortLabels <- settings$savedTrajectoryStates
    stateCohortPriorityOrder <- settings$savedPriorityOrder
    stateCohortMandatory <- settings$savedMandatoryStates
    stateCohortAbsorbing <- settings$savedAbsorbingStates
    stateSelectionType <- settings$savedStateSelectionType
    oocFix <- settings$outOfCohortFix
    trajectoryType <-
      if (settings$savedTrajectoryType == "Discrete") {
        0
      }
    else {
      1
    }
    lengthOfStay <- settings$savedLengthOfStay
    outOfCohortAllowed <- settings$savedOutOfCohortAllowed
    
    data <- DatabaseConnector::querySql(connection, sql)
    # data <- dplyr::arrange(data, SUBJECT_ID, COHORT_START_DATE)
    # Apply state names
    data$COHORT_DEFINITION_ID <- plyr::mapvalues(
      x = data$COHORT_DEFINITION_ID,
      from = 1:length(stateNamesJSON),
      to = stateNamesJSON,
      warn_missing = FALSE
    )
    data <- dplyr::select(data,
                          SUBJECT_ID,
                          COHORT_DEFINITION_ID,
                          COHORT_START_DATE,
                          COHORT_END_DATE)
    
  }
  else if (useCDM) {
    ParallelLogger::logInfo("Importing data ...")
    data <- getCohortData(
      connection,
      dbms,
      resultsSchema = cdmResultsSchema,
      cdmSchema = cdmSchema,
      selectedTarget = atlasTargetCohort,
      selectedStates = atlasStateCohorts,
      baseUrl,
      pathToResults = pathToResults
    )
    # Change state labels
    data$COHORT_DEFINITION_ID <- plyr::mapvalues(
      x = data$COHORT_DEFINITION_ID,
      from = c("0", as.character(atlasStateCohorts)),
      to = c("0", stateCohortLabels),
      warn_missing = FALSE
    )
    
    if (!is.null(baseUrl)) {
      for (i in 1:length(stateCohortLabels)) {
        file.rename(
          paste(
            pathToResults,
            "/inst/JSON/",
            atlasStateCohorts[i],
            ".json",
            sep = ""
          ),
          paste(
            pathToResults,
            "/inst/JSON/",
            stateCohortLabels[i],
            ".json",
            sep = ""
          )
        )
        file.rename(
          paste(
            pathToResults,
            "/inst/SQL/",
            atlasStateCohorts[i],
            ".sql",
            sep = ""
          ),
          paste(
            pathToResults,
            "/inst/SQL/",
            stateCohortLabels[i],
            ".sql",
            sep = ""
          )
        )
      }
    }
    
    ParallelLogger::logInfo("Data import completed!")
    
    
  }
  else {
    ParallelLogger::logInfo("Importing data ...")
    data = readr::read_csv(pathToData)
    ParallelLogger::logInfo("Read complete!")
  }
  
  data <- dplyr::arrange(data, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE, COHORT_DEFINITION_ID)
  
  if(nrow(data) == 0){
    return(ParallelLogger::logInfo("There were no patients imported! Check your target cohort!"))
  }
  
  save_object(
    path =  paste(
      pathToResults,
      "/tmp/datasets/",
      studyName,
      "importedData.csv",
      sep = ""
    ),
    object = data
  )
  
  ParallelLogger::logInfo("Cleaning data ...")
  
  data <- cleanCohortData(
    cohortData = data,
    mandatoryStates = stateCohortMandatory,
    outOfCohortAllowed = as.logical(outOfCohortAllowed),
    mergeStates = mergeStates,
    mergeThreshold = mergeThreshold
  )
  
  # As we may have new state labels (if mergeStates = TRUE) we now will modify some settings:
  if (mergeStates) {
    stateCohortPriorityOrder <- ordered_combinations(stateCohortPriorityOrder, n = length(mergeTreshold) + 1)
    
    allowedStatesList_updated <-  lapply(names(allowedStatesList), function(state_name) {
      c(allowedStatesList[[state_name]], stateCohortPriorityOrder[grepl(state_name, stateCohortPriorityOrder)])
    })
    names(allowedStatesList_updated) <- names(allowedStatesList)
    
    for (state_name in stateCohortPriorityOrder) {
      allowedStatesList_updated[[state_name]] <- allowedStatesList_updated[[strsplit(state_name, split = "\\+")[[1]][1]]]
    }
    
    allowedStatesList <- allowedStatesList_updated
    
    stateCohortAbsorbing <- unique(unlist(lapply(stateCohortAbsorbing, function(state_name) {
      stateCohortPriorityOrder[grepl(state_name, stateCohortPriorityOrder)]
    })))
  }
  
  ParallelLogger::logInfo("Data cleaning completed!")
  
  ParallelLogger::logInfo("Generating trajectories ...")
  result <- NULL
  if (trajectoryType == 0) {
    result <- getTrajectoriesDiscrete(
      connection = connection,
      cohortData = data,
      stateDuration = lengthOfStay,
      pathToResults = pathToResults,
      oocFix = oocFix,
      stateSelection = stateSelectionType,
      statePriorityVector = stateCohortPriorityOrder,
      absorbingStates = stateCohortAbsorbing,
      studyName = studyName,
      addPersonalData = useCDM,
      allowedStatesList = allowedStatesList
    )
  }
  else if (trajectoryType == 1) {
    result <- getTrajectoriesContinuous(
      connection = connection,
      patientData =  data,
      pathToResults = pathToResults,
      stateSelection = stateSelectionType,
      statePriorityVector = stateCohortPriorityOrder,
      absorbingStates = stateCohortAbsorbing,
      studyName = studyName,
      addPersonalData = useCDM,
      allowedStatesList = allowedStatesList
    )
  }
  
  
  ParallelLogger::logInfo("Trajectory generation completed!")
  
  ParallelLogger::logInfo("Saving trajectories to the specified temp schema ...")
  
  dropRelation(connection = connection, dbms = dbms, schema = cdmTmpSchema, relationName = paste(studyName, "patient_trajectories", sep = "_"))
  
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste(studyName, "patient_trajectories", sep = "_"),
                                 databaseSchema = cdmTmpSchema,
                                 data = result)
  
  ParallelLogger::logInfo("Trajectories saved to the specified temp schema!")
  
  ############################################################################
  #
  # Saving study settings as new row
  #
  ############################################################################
  
  if (!runSavedStudy) {
    savedTrajectoryType <- if (trajectoryType == 0) {
      "Discrete"
    }
    else {
      "Continuous"
    }
    savedTrajectoryStates <- stateCohortLabels
    savedPriorityOrder <- stateCohortPriorityOrder
    savedStateSelectionType <- stateSelectionType
    savedAbsorbingStates <- stateCohortAbsorbing
    savedMandatoryStates <- stateCohortMandatory
    savedLengthOfStay <- lengthOfStay
    savedOutOfCohortAllowed <- as.logical(outOfCohortAllowed)
    savedOutOfCohortFix <- oocFix
    # defining a row
    newSettings <- data.frame(
      studyName,
      savedTrajectoryType,
      paste(savedTrajectoryStates, collapse = ","),
      paste(savedPriorityOrder, collapse = ","),
      as.integer(savedStateSelectionType),
      paste(savedAbsorbingStates, collapse = ","),
      paste(savedMandatoryStates, collapse = ","),
      savedLengthOfStay,
      savedOutOfCohortAllowed,
      savedOutOfCohortFix
    )
    
    settings <-
      read.csv(paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ))
    if (studyName %in% settings$studyName) {
      studyIndex <- which(settings$studyName == studyName)
      settings[studyIndex,] <- newSettings
    }
    else {
      colnames(newSettings) <- colnames(settings)
      settings <- rbind(settings, newSettings)
    }
    
    write.csv(
      settings,
      paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ),
      row.names = FALSE
    )
    ParallelLogger::logInfo(paste(
      "Saved settings to: ",
      paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ),
      sep = ""
    ))
  }
  
  
  
  return(ParallelLogger::logInfo("Trajectories generated!"))
}


