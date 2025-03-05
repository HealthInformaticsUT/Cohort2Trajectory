#' C2T configuration for initiating the parameter values in the study environment
#' 
#' @param cdm object created with CDMConnector 
#' @param studyName name chosen for study, is used as a folder name
#' @param baseUrl the base URL for the WebApi instance
#' @param pathToStudy path to folder which contains folder named after studyName
#' @param atlasTargetCohort the id of the target cohort defined in OHDSI tool ATLAS
#' @param atlasStateCohorts the ids of the state cohorts defined in OHDSI tool ATLAS
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
#' @param trajectoryDataObject When using without OMOP CDM specify the data file  (if specified no need for pathToData)
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE the states will be label-merged given they overlap more than the specified threshold. Can be given as vector, then multiple iterations are runned,
#' @param pathToData When using without OMOP CDM specify the path to data file (if specified no need for trajectoryDataObject)
#' @param batchSize customizable batch size for cohort generation 
#'
#' @export
#' @examples
#' \dontrun{cohort2TrajectoryConfiguration(cdm = cdm,
#' studyName = "TestCohort2Trajectory",
#' baseUrl = NULL,
#' pathToStudy = getwd(),
#' atlasTargetCohort = 0,
#' atlasStateCohorts = c(1, 2),
#' stateCohortLabels = c("test_label1", "test_label2"),
#' )}
cohort2TrajectoryConfiguration <- function(cdm = NULL,
                                           
                                           studyName = "Cohort2Trajectory",
                                           baseUrl = "http://localhost:8080/WebAPI",
                                           
                                           pathToStudy = getwd(),
                                           
                                           atlasTargetCohort = NULL,
                                           atlasStateCohorts = NULL,
                                           stateCohortLabels = NULL,
                                           stateCohortPriorityOrder = NULL,
                                           stateCohortMandatory = NULL,
                                           stateCohortAbsorbing = NULL,
                                           stateSelectionType = NULL,
                                           
                                           oocFix = "None",
                                           trajectoryType = "Discrete",
                                           lengthOfStay = NULL,
                                           outOfCohortAllowed = FALSE,
                                           
                                           runSavedStudy = FALSE,
                                           useCDM = TRUE,
                                           trajectoryDataObject = NULL,
                                           pathToData = './Cohort2Trajectory/Data/importedData.csv',
                                           
                                           allowedStatesList = createStateList(stateCohortLabels),
                                           mergeStates = FALSE,
                                           mergeThreshold = 0.5,
                                           
                                           batchSize = 1000) {
  
  if (studyName %in% list.dirs(pathToStudy, full.names = FALSE, recursive = FALSE)) {
    cli::cli_warn("{.warning Study name already in use, consider renaming!}")
  }
  createMandatorySubDirs(pathToStudy, studyName)
  
  if (useCDM){
      if (!all((grepl("[a-zA-Z]", substr(stateCohortLabels,1,1))))){
        cli::cli_abort("{.error State labels necessary. Use state cohort labels that begin with a letter.}")
        return(NULL)
    }
  }
  
  if(is.data.frame(trajectoryDataObject)){
    if(class(trajectoryDataObject$cohort_start_date) != 'Date'){
      cli::cli_warn("{.warning cohort_start_date column is not in Date format! Convert using <as.Date> function.}")
    }
    if(class(trajectoryDataObject$cohort_end_date) != 'Date'){
      cli::cli_warn("{.warning cohort_end_date column is not in Date format! Convert using <as.Date> function.}")
    }
    if(class(trajectoryDataObject$subject_id) != 'numeric'){
      cli::cli_warn("{.warning subject_id column is not in numeric format! Convert using <as.integer> function.}")
    }
    if(class(trajectoryDataObject$cohort_definition_id) != 'character'){
      cli::cli_warn("{.warning cohort_definition_id column is not in character format! Convert using <as.character> function.}")
    }
  }
  
  if (!is.null(stateSelectionType)) {
    stateSelectionType = switch(
      stateSelectionType,
      "First" = 1,
      "Overlap" = 2,
      "Priority" = 3
    )
  }
  
  if (!is.null(trajectoryType)) {
    trajectoryType = switch(trajectoryType,
                            "Discrete" = 0,
                            "Continuous" = 1)
  }
  
  #studyName <<- NULL
  # Set study environment
  studyEnv <- rlang::env(
    # general
    studyName = studyName,
    pathToStudy = pathToStudy,

    # data importing
    runSavedStudy = runSavedStudy,
    useCDM = useCDM,
    trajectoryDataObject = trajectoryDataObject,
    pathToData = pathToData,
    baseUrl = baseUrl,
    atlasTargetCohort = atlasTargetCohort,
    atlasStateCohorts = atlasStateCohorts,

    # data cleaning
    allowedStatesList = allowedStatesList,
    mergeStates = mergeStates,
    mergeThreshold = mergeThreshold,

    stateCohortLabels = stateCohortLabels,
    stateCohortPriorityOrder = stateCohortPriorityOrder,
    stateCohortMandatory = stateCohortMandatory,
    stateCohortAbsorbing = stateCohortAbsorbing,
    stateSelectionType = stateSelectionType,

    # trajectory generation
    oocFix = oocFix,
    trajectoryType = trajectoryType,
    lengthOfStay = lengthOfStay,
    outOfCohortAllowed = outOfCohortAllowed,

    batchSize = batchSize
  )
  
  return(studyEnv)
}
