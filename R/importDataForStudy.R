#' Import and clean cohort intervals for a study
#'
#' Retrieve cohort data from an OMOP CDM or read local cohort intervals, then
#' apply observation bounds, mandatory-state requirements and optional state
#' merging. The analyst defines the target and clinical state cohorts.
#'
#' @param cdm A CDM reference created with CDMConnector.
#' @param studyEnv Study environment returned by cohort2TrajectoryConfiguration.
#' @param useCDM Whether to retrieve cohorts from the CDM. Use FALSE for a
#'   local data frame or CSV file.
#' @param baseUrl OHDSI WebAPI base URL for downloading ATLAS definitions.
#'   Use NULL to read local JSON definitions.
#' @param atlasTargetCohort ATLAS target cohort ID, or local definition name
#'   without the .json extension when baseUrl is NULL.
#' @param atlasStateCohorts ATLAS state cohort IDs, or local definition names
#'   without the .json extension when baseUrl is NULL.
#' @param stateCohortLabels Nonblank, non-missing clinical state labels, unique
#'   ignoring case. The labels "0", "START", "EXIT" and "OUT OF COHORT" are
#'   reserved, also ignoring case.
#' @param stateCohortPriorityOrder State labels ordered from highest to lowest
#'   priority, also used to resolve inherited priorities for merged states.
#' @param stateCohortMandatory States that every included patient must have
#'   within the retained observation bounds. NULL or character(0) imposes no
#'   requirements. Rerun import to change these requirements before generation.
#' @param stateCohortAbsorbing Labels of absorbing states, also used to derive
#'   absorbing status for merged states.
#' @param allowedStatesList Named list of allowed destinations by source state.
#'   Each source name must be unique, nonempty and non-missing.
#' @param outOfCohortAllowed Whether to retain state records beyond the target
#'   interval's end. The target start remains the lower boundary.
#' @param mergeStates Whether to compose overlapping state intervals into
#'   states with structured membership.
#' @param mergedStateOverrides List of mergedState() definitions that replace
#'   selected inherited properties of merged states.
#' @param trajectoryDataObject Data frame for a local import. An explicitly
#'   supplied non-NULL object overrides the configured CSV path. Supply NULL
#'   to use the CSV path. Overrides apply to this import only.
#' @param pathToData CSV path for a local import. An explicitly supplied
#'   non-NULL path overrides the configured data object. Do not explicitly
#'   supply both non-NULL local sources.
#' @param runSavedStudy Whether to import fresh data using the analysis rules
#'   restored by a saved-study configuration.
#'
#' @details
#' The target cohort defines observation bounds. Clinical state intervals,
#' including history states and finite episodes, must be defined by the
#' analyst before import. State labels are preserved for CDM, data-frame and
#' CSV input. Spaces and punctuation are not replaced to make filenames.
#' Use the same labels in mandatory states, priorities and transition rules.
#' CSV cohort_definition_id values are read as text, preserving leading zeros
#' and the literal label "NA". Empty labels are invalid. All mandatory-state
#' labels are literal, including "No mandatory state".
#'
#' A nonempty target cohort remains valid when no clinical state records
#' survive cleaning, provided mandatory-state requirements do not exclude its
#' patients. The target intervals still define follow-up. Discrete trajectories
#' use the configured gap rule. Continuous trajectories retain boundaries
#' without inventing clinical states.
#'
#' An empty eligible cohort is not a successful import. Previous cleaned data,
#' merged-state artifacts and saved settings remain unchanged. An unsuccessful
#' import blocks trajectory generation until a later import succeeds.
#'
#' @section Local input:
#' With neither local source explicitly supplied, the configured data object
#' is used when present, otherwise the configured CSV path is used. An explicit
#' non-NULL source overrides the opposite configured source for this call only.
#' Supplying trajectoryDataObject = NULL selects the CSV path. The same rules
#' apply to saved local studies. CDM imports ignore both local source arguments.
#'
#' @section Cohort definitions and saved studies:
#' Fresh CDM imports stage definitions and their manifest in a temporary
#' folder. Downloaded definitions and the manifest are published after data
#' retrieval, cleaning and saving succeed. Failed downloads, queries or empty
#' eligible cohorts leave the previous working JSON files unchanged. Existing
#' local definitions and unrelated JSON files are not rewritten.
#'
#' Successful CDM source overrides are retained for subsequent imports, keeping
#' target and state selectors and the ATLAS URL aligned with clinical labels.
#' Failed attempts retain the previous source settings. Downloaded definitions
#' use 0.json for the target and atlas_<id>.json for states. Repeated state IDs
#' share a file without changing their clinical labels.
#'
#' Successful import captures the selected definition filenames and contents.
#' After fresh trajectory generation completes, these are saved with the
#' analysis settings in Settings/analysisSettings.rds. Only version 4 snapshots
#' are accepted. The snapshot contains no patient data.
#'
#' Saved CDM runs write the embedded definitions to a new private folder and
#' use them to query fresh data. They do not read or replace working JSON files
#' or their manifest. A later fresh import cannot change the definitions in an
#' earlier completed snapshot. Saved local runs require fresh local input.
#' Neither saved mode accepts overrides to its restored analysis rules.
#'
#' @return Invisibly returns NULL after a successful import. Cleaned data are
#'   saved in the study's Data directory and studyEnv is updated for generation.
#' @seealso \code{vignette("a02_import", package = "Cohort2Trajectory")},
#'   \code{vignette("a05_output", package = "Cohort2Trajectory")}
#' @export
#' @examples
#' \dontrun{
#' getDataForStudy(cdm = cdm, studyEnv = studyEnv)
#' }
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
                            mergedStateOverrides = studyEnv$mergedStateOverrides) {
  
  # Variables from global studyEnv (can't be altered)
  studyName = studyEnv$studyName
  pathToStudy = studyEnv$pathToStudy
  studyEnv$.dataReady <- FALSE
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
  .validateAllowedStateSources(allowedStatesList)
  importSettings <- mget(.studyImportFields(), envir = environment(), inherits = FALSE)
  definitions <- NULL
  definitionContents <- NULL

  if (useCDM) {
    # Keep downloads and manifest updates away from the last saved study until
    # the import succeeds. A fixed staging name also keeps the copy self-contained.
    definitionStage <- tempfile("c2t-definitions-")
    if (!dir.create(definitionStage)) stop("Could not create the cohort-definition staging folder.", call. = FALSE)
    on.exit(unlink(definitionStage, recursive = TRUE), add = TRUE)
    dir.create(file.path(definitionStage, "study"))
    jsonPath <- file.path(pathToStudy, studyName, "JSON")
    if (runSavedStudy) {
      .stageSavedDefinitions(saved, file.path(definitionStage, "study", "JSON"))
    } else if (!file.copy(jsonPath, file.path(definitionStage, "study"), recursive = TRUE)) {
      stop("Could not stage the study's JSON folder. Check that it exists and is readable.", call. = FALSE)
    }
  }

  cli::cli_progress_step("Importing data ...")
  if (runSavedStudy && useCDM) {
    definitions <- saved$definitions
    data <- getCohortData(
      cdm = cdm,
      studyEnv = studyEnv,
      selectedTarget = definitions$target,
      selectedStates = definitions$states,
      baseUrl = NULL,
      pathToStudy = definitionStage,
      studyName = "study",
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
      pathToStudy = definitionStage,
      studyName = "study",
      stateCohortLabels = stateCohortLabels
    )

    # Preserve both the final clinical labels and the filenames used by the
    # downloader and manifest. Renaming files to labels breaks repeated IDs,
    # labels containing path separators, and subsequent saved-study imports.
    definitions <- .cohortDefinitionFiles(atlasTargetCohort, atlasStateCohorts, baseUrl)

  } else {
    # Explicit local inputs override the opposite configured source without
    # changing which source a later import uses by default.
    explicitObject <- !missing(trajectoryDataObject) && !is.null(trajectoryDataObject)
    explicitPath <- !missing(pathToData) && !is.null(pathToData)
    if (explicitObject && explicitPath) {
      stop("Supply only one non-NULL local input: trajectoryDataObject or pathToData.",
           call. = FALSE)
    }
    if (explicitPath || is.null(trajectoryDataObject)) {
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

  if (useCDM) {
    definitionContents <- .readDefinitionContents(
      definitions, file.path(definitionStage, "study", "JSON")
    )
  }
  
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
  # Reject an empty eligible cohort before replacing any cleaned-data or
  # merged-state artifacts from the previous successful import.
  if (nrow(data) == 0L) {
    cli::cli_warn("{.warning No patients left after cleaning the data!}")
    return(invisible(NULL))
  }
  
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

  if (useCDM && !runSavedStudy) {
    publish <- "CohortsToCreate.csv"
    if (!is.null(baseUrl)) {
      publish <- c(publish, paste0(unique(c(definitions$target, definitions$states)), ".json"))
    }
    # Fresh local definitions were only read. Publish downloads and the new
    # manifest without replacing unrelated files. Saved runs publish neither.
    copied <- file.copy(file.path(definitionStage, "study", "JSON", publish),
                        jsonPath, overwrite = TRUE)
    if (!all(copied)) {
      stop("Could not save cohort-definition files: ", paste(publish[!copied], collapse = ", "), call. = FALSE)
    }
  }
  
  # Target-only histories are valid after mandatory-state filtering. Lack of
  # recorded states does not erase the target interval or its observed time.
  # Generation must use the same rules that were used/expanded during import.
  studyEnv$.importSettings <- importSettings
  studyEnv$.cohortDefinitions <- definitions
  studyEnv$.cohortDefinitionContents <- definitionContents
  studyEnv$atlasTargetCohort <- atlasTargetCohort
  # Keep source selectors aligned with the imported target and clinical labels.
  # Saved imports already use their restored cohort-definition mapping.
  if (useCDM && !runSavedStudy) {
    studyEnv$atlasStateCohorts <- atlasStateCohorts
    studyEnv$baseUrl <- baseUrl
  }
  studyEnv$useCDM <- useCDM
  list2env(mget(.studyImportFields(), envir = environment(), inherits = FALSE),
           envir = studyEnv)
  if (isTRUE(runSavedStudy)) {
    list2env(saved$generation[.studyGenerationFields()], envir = studyEnv)
  }
  studyEnv$.dataReady <- TRUE
  invisible(NULL)
}
