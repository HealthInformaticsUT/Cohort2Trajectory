#' C2T configuration for initiating the parameter values in the study environment
#' 
#' @param cdm object created with CDMConnector 
#' @param studyName name chosen for study, is used as a folder name
#' @param baseUrl the base URL for the WebApi instance
#' @param pathToStudy path to folder which contains folder named after studyName
#' @param atlasTargetCohort the id of the target cohort defined in OHDSI tool ATLAS
#' @param atlasStateCohorts the ids of the state cohorts defined in OHDSI tool ATLAS
#' @param stateCohortLabels Character vector of clinical state labels, unique
#'   even when ignoring case. Labels must not be blank or missing. The labels
#'   "0", "START", "EXIT" and "OUT OF COHORT" are reserved (case-insensitively).
#' @param stateCohortPriorityOrder vector of the customized labels of the state cohorts in priority order
#' @param stateCohortMandatory Vector of state labels required in a trajectory.
#'   Patients must have every supplied state within the retained observation
#'   bounds. NULL or character(0) means no mandatory states. All supplied labels
#'   are literal, including the former placeholder "No mandatory state".
#' @param stateCohortAbsorbing Vector of absorbing state labels.
#'   NULL or character(0) means no absorbing states.
#' @param stateSelectionType the type of state selection ("First" - First occurring, "Overlap" - Max overlap, "Priority" - Priority). Names are case-insensitive. Legacy numeric codes 1, 2 and 3 are accepted with a deprecation warning.
#' @param oocFix the method to use for replacing "OUT OF COHORT" states with more relevant states ("None" -> "OUT OF COHORT"; "Last present state" -> repeat the last one; random str -> used as state)
#' @param trajectoryType The type of the trajectory ("Discrete" - Discrete time, "Continuous" - Continuous time). Names are case-insensitive. Legacy numeric codes 0 and 1 are accepted with a deprecation warning.
#' @param lengthOfStay Positive whole-number cycle length in days for discrete
#'   trajectories. NULL may be supplied during configuration, but a cycle length
#'   is required when generating discrete trajectories. The final
#'   cycle is shortened if the trajectory boundary falls partway through it.
#' @param outOfCohortAllowed Whether retained state records may extend the
#'   trajectory beyond the target interval's end. The target start is unchanged.
#' @param runSavedStudy Restore the complete analysis settings from
#'   studyName/Settings/analysisSettings.rds. Supply the new parent folder and
#'   study name, not the analysis settings again. Connections are supplied to
#'   getDataForStudy and createTrajectories. Older CSV-only studies must first
#'   be rerun with their original configuration to save the complete settings.
#' @param useCDM The package can also be run without the OMOP CDM
#' @param trajectoryDataObject When using without OMOP CDM specify the data file  (if specified no need for pathToData)
#' @param allowedStatesList A list object which indicates accessible states from said state
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergedStateOverrides List of mergedState() definitions. Omitted
#'   properties inherit from the original member states.
#' @param ... Compatibility only: legacy named mergeThreshold is ignored with
#'   a warning. No overlap threshold is used.
#' @param pathToData When using without OMOP CDM specify the path to data file (if specified no need for trajectoryDataObject)
#' @param batchSize Maximum number of patients per trajectory-generation batch.
#'   Must be a single positive whole number. Defaults to 1000.
#' @details For discrete trajectories, selection ranks only overlapping states
#'   allowed from the last selected state. First chooses the earliest start,
#'   Overlap chooses the greatest overlap, and Priority follows the priority
#'   vector. Date intervals include both endpoints, so a shared boundary date
#'   counts as one day of overlap. Ties use priority order. If no candidate
#'   qualifies, oocFix is used.
#'   All three discrete selection methods require every clinical state within
#'   the trajectory boundaries to appear in stateCohortPriorityOrder. An omitted
#'   state raises an error rather than being converted into an unresolved cycle.
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
#'   With mergeStates = TRUE, exact overlapping intervals form states with
#'   structured membership before discretisation. Labels are never parsed.
#'   Destinations are inherited by union. Every newly introduced destination
#'   member must be permitted by at least one source member. Self-transitions
#'   and transitions only removing members require permission for all retained
#'   destination members. A combination is absorbing if any member is absorbing.
#'   Explicit mergedState() properties replace inheritance. Absorbing status
#'   takes precedence over outgoing transitions. The stateDictionary in the
#'   study environment and Data/stateDictionary.rds describes the resolved states.
#'   Patient IDs may be numeric whole numbers, character or integer64 values.
#'   Temporary integer IDs are assigned internally and originals are restored
#'   before output. No manual remapping is needed. For in-memory IDs at or above
#'   2^53, use character or integer64 input to avoid prior rounding by doubles.
#'   Missing and blank identifiers are rejected.
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
                                           batchSize = 1000,
                                           mergedStateOverrides = list(),
                                           ...) {
  .legacyMergeArguments(list(...))
  if (isTRUE(runSavedStudy)) {
    .checkSavedOverrides(match.call(), c(.studyImportFields(),
      .studyGenerationFields(), "atlasTargetCohort", "atlasStateCohorts", "baseUrl"))
    .validatePositiveInteger(batchSize, "batchSize", "patients")
    saved <- .readStudySnapshot(pathToStudy, studyName)
    if (useCDM) .checkSavedDefinitions(saved, pathToStudy, studyName)
    createMandatorySubDirs(pathToStudy, studyName)
    studyEnv <- rlang::env(
      studyName = studyName, pathToStudy = pathToStudy, runSavedStudy = TRUE,
      useCDM = useCDM, trajectoryDataObject = trajectoryDataObject,
      pathToData = pathToData, batchSize = batchSize, baseUrl = NULL,
      atlasTargetCohort = "0", atlasStateCohorts = saved$definitions$states,
      .savedSettings = saved, .dataReady = FALSE
    )
    list2env(saved$import[.studyImportFields()], envir = studyEnv)
    list2env(saved$generation[.studyGenerationFields()], envir = studyEnv)
    return(studyEnv)
  }
  # Validate choices before creating study files. NULL remains available for
  # workflows that supply these settings later or load a saved study.
  stateSelectionType <- .configurationChoice(
    stateSelectionType, c(First = 1, Overlap = 2, Priority = 3), "stateSelectionType"
  )
  trajectoryType <- .configurationChoice(
    trajectoryType, c(Discrete = 0, Continuous = 1), "trajectoryType"
  )
  if (isTRUE(trajectoryType == "Discrete") && !is.null(lengthOfStay)) {
    .validatePositiveInteger(lengthOfStay, "lengthOfStay", "days")
  }
  .validatePositiveInteger(batchSize, "batchSize", "patients")
  .validateMergeOptions(mergeStates, mergedStateOverrides)
  .validateStateLabels(stateCohortLabels)
  
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
    if(!(is.numeric(trajectoryDataObject$subject_id) || is.character(trajectoryDataObject$subject_id))){
      cli::cli_warn("{.warning subject_id column must contain numeric, character or integer64 identifiers.}")
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
    mergedStateOverrides = mergedStateOverrides,

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

.configurationChoice <- function(value, choices, argument, allowNull = TRUE, warnCodes = TRUE) {
  if (is.null(value) && allowNull) return(NULL)
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    index <- match(tolower(trimws(value)), tolower(names(choices)))
    if (!is.na(index)) return(names(choices)[index])
  }
  if (is.numeric(value) && !is.complex(value) && length(value) == 1L &&
      is.finite(value) && value %in% unname(choices)) {
    name <- names(choices)[match(value, choices)]
    if (warnCodes) {
      warning(argument, " = ", value, " is deprecated. Use ", argument,
              ' = "', name, '" instead.', call. = FALSE)
    }
    return(name)
  }
  stop(argument, " must be one of: ", paste(names(choices), collapse = ", "),
       ".", call. = FALSE)
}

.validatePositiveInteger <- function(value, argument, unit) {
  if (!is.numeric(value) || is.complex(value) || length(value) != 1L || !is.finite(value) ||
      value < 1 || value > .Machine$integer.max || value != trunc(value)) {
    stop(argument, " must be a single positive whole number of ", unit,
         " within the integer range.",
         call. = FALSE)
  }
  invisible(NULL)
}
