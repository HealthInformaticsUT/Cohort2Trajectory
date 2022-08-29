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
#' @param cdmTmpSchema Schema for temporary tables, will be deleted.
#' @param cdmResultsSchema Schema which has the information about the cohorts created in Atlas
#' @param atlasTargetCohort The id of the target cohort defined in OHDSI tool ATLAS
#' @param atlasStateCohorts The ids of the state cohorts defined in OHDSI tool ATLAS
#' @param stateCohortLabels Vector of the customized labels of the state cohorts
#' @param stateCohortPriorityOrder Vector of the customized labels of the state cohorts in priority order
#' @param stateCohortMandatory Vector of the customized labels of the state cohorts which are mandatory in trajectory
#' @param stateCohortAbsorbing Vector of the customized labels of the state cohorts which are absorbing
#' @param stateSelectionType The type of state selection (1 - First occurring, 2 - Max overlap, 3 - Priority)
#' @param trajectoryType The type of the trajectory (0 - Discrete time, 1 - Continuous time)
#' @param lengthOfStay The length of stay (days) in one state (Effect only in discrete case)
#' @param outOfCohortAllowed boolean whether the patient trajectory can surpass the target cohort's observation-period
#' @example man/examples/Cohort2Trajectory.R
#'
#' @export
Cohort2Trajectory <- function(dbms = "postgresql",
                             connection,
                             cdmSchema = "ohdsi_cdm",
                             cdmTmpSchema = "ohdsi_temp",
                             cdmResultsSchema = "ohdsi_results",
                             studyName = "Cohort2Trajectory",
                             baseUrl = "http://localhost:8080/WebAPI",
                             atlasTargetCohort,
                             atlasStateCohorts,
                             stateCohortLabels,
                             stateCohortPriorityOrder,
                             stateCohortMandatory,
                             stateCohortAbsorbing,
                             stateSelectionType,
                             trajectoryType,
                             lengthOfStay,
                             outOfCohortAllowed,
                             runSavedStudy = FALSE,
                             pathToResults = getwd())
{
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
    CohortGenerator::generateCohortSet(
      connection = connection,
      cdmDatabaseSchema = cdmSchema,
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
    trajectoryType <- if (settings$savedTrajectoryType == "Discrete") {
      0
    }
    else {
      1
    }
    lengthOfStay <- settings$savedLengthOfStay
    outOfCohortAllowed <- settings$savedOutOfCohortAllowed
    
    
    data <- DatabaseConnector::querySql(connection, sql)
    # Apply state names
    names <- c("0", stateCohortPriorityOrder)
    data$COHORT_DEFINITION_ID <- plyr::mapvalues(
      x = data$COHORT_DEFINITION_ID,
      from = 1:length(names),
      to = names,
      warn_missing = FALSE
    )
    data <- dplyr::select(data,
                         SUBJECT_ID,
                         COHORT_DEFINITION_ID,
                         COHORT_START_DATE,
                         COHORT_END_DATE)
    
  }
  else {
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
        paste(pathToResults,
              "/inst/SQL/",
              atlasStateCohorts[i],
              ".sql",
              sep = ""),
        paste(pathToResults,
              "/inst/SQL/",
              stateCohortLabels[i],
              ".sql",
              sep = "")
      )
    }
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
    
    
    ParallelLogger::logInfo("Data import completed!")
    
    
  }
  
  
  
  ParallelLogger::logInfo("Cleaning data ...")
  data <- cleanCohortData(
    cohortData = data,
    mandatoryStates = stateCohortMandatory,
    outOfCohortAllowed = as.logical(outOfCohortAllowed)
    
  )
  ParallelLogger::logInfo("Data cleaning completed!")
  
  
  ParallelLogger::logInfo("Generating trajectories ...")
  
  result <- NULL
  if (trajectoryType == 0) {
    result <- getTrajectoriesDiscrete(
      connection = connection,
      cohortData = data,
      stateDuration = lengthOfStay,
      pathToResults = pathToResults,
      stateSelection = stateSelectionType,
      statePriorityVector = stateCohortPriorityOrder,
      absorbingStates = stateCohortAbsorbing,
      studyName = studyName
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
      studyName = studyName
    )
  }

  
  ParallelLogger::logInfo("Trajectory generation completed!")
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
      savedOutOfCohortAllowed
    )
    
    settings <- read.csv(paste(pathToResults, "/inst/Settings/trajectorySettings.csv", sep = ""))
    if (studyName %in% settings$studyName) {
      studyIndex <- which(settings$studyName == studyName)
      settings[studyIndex, ] <- newSettings
    }
    else{
      colnames(newSettings) <- colnames(settings)
      settings <- rbind(settings, newSettings)
    }
    
    write.csv(
      settings,
      paste(pathToResults, "/inst/Settings/trajectorySettings.csv", sep = ""),
      row.names = FALSE
    )
    ParallelLogger::logInfo(paste(
      "Saved settings to: ",
      paste(pathToResults, "/inst/Settings/trajectorySettings.csv", sep = ""),
      sep = ""
    ))
  }
  
  
  return(ParallelLogger::logInfo("Trajectories generated!"))
}