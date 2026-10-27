#' Import the data from CDM for the study
#'
#' 
#' @param cdm object created with CDMConnector 
#' @param baseUrl the base URL for the WebApi instance
#' @param atlasTargetCohort the id of the target cohort defined in OHDSI tool ATLAS
#' @param atlasStateCohorts the ids of the state cohorts defined in OHDSI tool ATLAS
#' @param stateCohortLabels Character vector of clinical state labels, unique
#'   even when ignoring case. Labels must not be blank or missing. The labels
#'   "0", "START", "EXIT" and "OUT OF COHORT" are reserved (case-insensitively).
#'   CSV cohort_definition_id values are read as text, including leading zeros
#'   and the literal label "NA". Empty CSV labels are invalid.
#' @param stateCohortPriorityOrder vector of the customized labels of the state cohorts in priority order
#' @param stateCohortMandatory Vector of state labels patients must all have
#'   within the retained observation bounds. NULL or character(0) imposes no
#'   requirements. All labels are literal, including "No mandatory state".
#'   Rerun this import to add or remove requirements before creating trajectories.
#' @param outOfCohortAllowed boolean whether the patient trajectory can surpass the target cohort's observation-period
#' @param runSavedStudy Use the analysis rules restored by a saved-study
#'   configuration and import fresh data from the supplied connection or input.
#' @param useCDM The package can also be run without the OMOP CDM
#' @param trajectoryDataObject When using without OMOP CDM specify the data file  (if specified no need for pathToData)
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergedStateOverrides List of mergedState() definitions.
#' @param ... Compatibility only: legacy named mergeThreshold warns and is ignored.
#' @param pathToData When using without OMOP CDM specify the path to data file (if specified no need for trajectoryDataObject)
#' @param allowedStatesList List of allowed transitions between states
#' @param stateCohortAbsorbing Vector of absorbing states
#' @param studyEnv environment created with cohort2TrajectoryConfiguration 
#'
#' @details A nonempty target cohort remains valid when no state events survive
#'   cleaning, provided no mandatory-state requirement excludes those patients.
#'   Their target intervals still define follow-up. Discrete trajectories use
#'   the configured gap rule, and continuous trajectories retain boundaries
#'   without inventing clinical states. An empty eligible cohort is not treated
#'   as a successful import.
#'   State labels are preserved as supplied for CDM, data-frame and CSV input.
#'   Spaces and punctuation are not converted
#'   into filename-safe substitutes. Use the same labels in state definitions,
#'   mandatory states, priorities and transition rules.
#'   Downloaded ATLAS definitions use 0.json for the target and atlas_<id>.json
#'   for states. Repeated state IDs share a definition file without changing
#'   their clinical labels. Saved settings retain this filename mapping.
#'
#' @return log info
#' @export
#' @examples
#' \dontrun{getDataForStudy(cdm = cdm,studyEnv = studyEnv)}
getDataForStudy <- function(runSavedStudy = studyEnv$runSavedStudy,
                            useCDM = studyEnv$useCDM,
                            trajectoryDataObject = studyEnv$trajectoryDataObject,
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
                            allowedStatesList = studyEnv$allowedStatesList,
                            stateCohortAbsorbing = studyEnv$stateCohortAbsorbing,
                            pathToData = studyEnv$pathToData,
                            mergedStateOverrides = studyEnv$mergedStateOverrides,
                            ...)  {
  
  # Variables from global studyEnv (can't be altered)
  studyName = studyEnv$studyName
  pathToStudy = studyEnv$pathToStudy
  studyEnv$.dataReady <- FALSE
  .legacyMergeArguments(list(...))
  if (isTRUE(runSavedStudy)) {
    .checkSavedOverrides(match.call(), c(.studyImportFields(),
      "atlasTargetCohort", "atlasStateCohorts", "baseUrl"))
    saved <- studyEnv$.savedSettings
    if (is.null(saved)) {
      stop("First configure the study with runSavedStudy = TRUE.", call. = FALSE)
    }
    list2env(saved$import[.studyImportFields()], envir = environment())
    if (useCDM) .checkSavedDefinitions(saved, pathToStudy, studyName)
    atlasTargetCohort <- "0"
  }
  .validateMergeOptions(mergeStates, mergedStateOverrides)
  .validateStateLabels(stateCohortLabels)
  importSettings <- mget(.studyImportFields(), envir = environment(), inherits = FALSE)
  definitions <- NULL

  cli::cli_progress_step("Importing data ...")
  if (runSavedStudy && useCDM) {
    definitions <- saved$definitions
    data <- getCohortData(
      cdm = cdm,
      studyEnv = studyEnv,
      selectedTarget = definitions$target,
      selectedStates = definitions$states,
      baseUrl = NULL,
      stateCohortLabels = stateCohortLabels
    )
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

    # Preserve both the final clinical labels and the filenames used by the
    # downloader and manifest. Renaming files to labels breaks repeated IDs,
    # labels containing path separators, and subsequent saved-study imports.
    definitions <- .cohortDefinitionFiles(atlasTargetCohort, atlasStateCohorts, baseUrl)

  } else {
    # get data from input
    if (is.null(trajectoryDataObject)) {
      data = .readPatientCsv(pathToData)
    } else {
      # Clinical labels are values, not filenames. Renaming them here can
      # collapse distinct states and break the configured selection rules.
      data <- trajectoryDataObject
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
  
  .validateObservedStateLabels(data$cohort_definition_id, stateCohortLabels)
  data <- cleanCohortData(
    cohortData = data,
    mandatoryStates = stateCohortMandatory,
    outOfCohortAllowed = outOfCohortAllowed
  )
  
  
  # Keep source rules unchanged. Only the resolved interval table uses state keys.
  registry <- NULL
  if (mergeStates) {
    sourceIntervals <- data
    merged <- .composeStateIntervals(data, stateCohortLabels, mergedStateOverrides)
    data <- merged$data
    registry <- .mergedRules(merged$registry, stateCohortPriorityOrder,
                             allowedStatesList, stateCohortAbsorbing,
                             mergedStateOverrides)$registry
    # Preserve bounded original-state evidence separately from the resolved view.
    saveRDS(sourceIntervals, file.path(pathToStudy, studyName, "Data", "sourceIntervals.rds"))
  }
  studyEnv$stateDictionary <- registry
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
  
  # Target-only histories are valid after mandatory-state filtering. Lack of
  # recorded states does not erase the target interval or its observed time.
  # Generation must use the same rules that were used/expanded during import.
  studyEnv$.importSettings <- importSettings
  studyEnv$.cohortDefinitions <- definitions
  studyEnv$atlasTargetCohort <- atlasTargetCohort
  studyEnv$useCDM <- useCDM
  list2env(mget(.studyImportFields(), envir = environment(), inherits = FALSE),
           envir = studyEnv)
  if (isTRUE(runSavedStudy)) {
    list2env(saved$generation[.studyGenerationFields()], envir = studyEnv)
  }
  studyEnv$.dataReady <- TRUE
  invisible(NULL)
}
