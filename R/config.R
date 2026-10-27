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
#' @param stateCohortMandatory Vector of state labels required in a trajectory.
#'   NULL or character(0) means no mandatory states.
#' @param stateCohortAbsorbing Vector of absorbing state labels.
#'   NULL or character(0) means no absorbing states.
#' @param stateSelectionType the type of state selection ("First" - First occurring, "Overlap" - Max overlap, "Priority" - Priority). Names are case-insensitive.
#' @param oocFix the method to use for replacing "OUT OF COHORT" states with more relevant states ("None" -> "OUT OF COHORT"; "Last present state" -> repeat the last one; random str -> used as state)
#' @param trajectoryType The type of the trajectory ("Discrete" - Discrete time, "Continuous" - Continuous time). Names are case-insensitive.
#' @param lengthOfStay Cycle length in days for discrete trajectories. The final
#'   cycle is shortened if the trajectory boundary falls partway through it.
#' @param outOfCohortAllowed Whether retained state records may extend the
#'   trajectory beyond the target interval's end. The target start is unchanged.
#' @param runSavedStudy running a predefined study from studyName/Settings/trajectorySettings.csv
#' @param useCDM The package can also be run without the OMOP CDM
#' @param trajectoryDataObject When using without OMOP CDM specify the data file  (if specified no need for pathToData)
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE the states will be label-merged given they overlap more than the specified threshold. Can be given as vector, then multiple iterations are runned,
#' @param pathToData When using without OMOP CDM specify the path to data file (if specified no need for trajectoryDataObject)
#' @param batchSize customizable batch size for cohort generation 
#' @details For discrete trajectories, selection ranks only overlapping states
#'   allowed from the last selected state. First chooses the earliest start,
#'   Overlap chooses the greatest overlap, and Priority follows the priority
#'   vector. Date intervals include both endpoints, so a shared boundary date
#'   counts as one day of overlap. Ties use priority order. If no candidate
#'   qualifies, oocFix is used.
#'   Carry-forward requires an allowed self-transition, and a named fallback
#'   must also be permitted. Otherwise the cycle is OUT OF COHORT. This unresolved
#'   marker preserves the last selected state for checking later transitions.
#'   In both trajectory formats, an empty allowedStatesList imposes no transition
#'   restrictions. In a nonempty list, each state used as a transition source
#'   must have an entry, with character(0) meaning no
#'   permitted destinations. Rules are applied independently for each patient.
#'   Continuous output checks transitions from the last retained clinical state
#'   and removes prohibited records before applying the absorbing-state cutoff.
#'   A rejected absorbing record does not end follow-up. No further transition
#'   rules are required after a retained absorbing state. Destination matching
#'   is case-insensitive in both formats.
#'   The first target-cohort interval (label 0 in cleaned data) defines trajectory
#'   boundaries, including for patients with no state records. Discrete gaps use
#'   oocFix. The legacy OUT OF COHORT label means no selected state, not loss of
#'   observation. Continuous output retains interval endpoints and boundary
#'   markers without filling undocumented gaps or shifting simultaneous events.
#'   Continuous Priority selection requires every clinical state in the bounded
#'   input to be listed in stateCohortPriorityOrder. Missing states raise an error.
#'   Models requiring unique timestamps must resolve ties explicitly.
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
  # Validate choices before creating study files. NULL remains available for
  # workflows that supply these settings later or load a saved study.
  stateSelectionType <- .configurationChoice(
    stateSelectionType, c(First = 1, Overlap = 2, Priority = 3), "stateSelectionType"
  )
  trajectoryType <- .configurationChoice(
    trajectoryType, c(Discrete = 0, Continuous = 1), "trajectoryType"
  )
  
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
    if(!inherits(trajectoryDataObject$cohort_start_date, 'Date')){
      cli::cli_warn("{.warning cohort_start_date column is not in Date format! Convert using <as.Date> function.}")
    }
    if(!inherits(trajectoryDataObject$cohort_end_date, 'Date')){
      cli::cli_warn("{.warning cohort_end_date column is not in Date format! Convert using <as.Date> function.}")
    }
    if(!is.numeric(trajectoryDataObject$subject_id)){
      cli::cli_warn("{.warning subject_id column is not in numeric format! Convert using <as.integer> function.}")
    }
    if(!is.character(trajectoryDataObject$cohort_definition_id)){
      cli::cli_warn("{.warning cohort_definition_id column is not in character format! Convert using <as.character> function.}")
    }
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

.configurationChoice <- function(value, choices, argument) {
  if (is.null(value)) return(NULL)
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    index <- match(tolower(trimws(value)), tolower(names(choices)))
    if (!is.na(index)) return(unname(choices[index]))
  }
  stop(argument, " must be one of: ", paste(names(choices), collapse = ", "),
       ".", call. = FALSE)
}
