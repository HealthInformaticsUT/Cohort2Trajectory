#
#' Generate patient trajectories from imported cohort intervals
#'
#' Apply the configured selection, transition and gap rules to a successful
#' study import. The analyst defines clinical states and their intervals.
#' The target cohort supplies the observation bounds.
#'
#' @param cdm A CDM reference created with CDMConnector, required when joining
#'   demographics with useCDM = TRUE.
#' @param studyEnv Study environment returned by cohort2TrajectoryConfiguration.
#' @param trajectoryType Trajectory format, either "Discrete" or "Continuous".
#'   Names are case-insensitive. Numeric choice codes are not accepted.
#' @param stateSelectionType Selection method, either "First", "Overlap" or
#'   "Priority". Names are case-insensitive. A method must be specified.
#' @param lengthOfStay Positive whole-number cycle length in days for discrete
#'   trajectories. Use 7 for weekly cycles. Continuous output does not use it.
#' @param stateCohortLabels Nonblank, non-missing clinical state labels, unique
#'   ignoring case. The labels "0", "START", "EXIT" and "OUT OF COHORT" are
#'   reserved, also ignoring case.
#' @param stateCohortPriorityOrder State labels ordered from highest to lowest
#'   priority. Required for all discrete methods and continuous Priority.
#' @param stateCohortMandatory Mandatory states used by the successful import.
#'   Rerun getDataForStudy to change them. Order and duplicates do not matter.
#'   NULL and character(0) both mean no requirements.
#' @param stateCohortAbsorbing Labels of states that end a trajectory when
#'   retained. In nonmerged studies, use the exact spelling of a configured
#'   state, observed state or named discrete fallback. NULL and character(0)
#'   mean no absorbing states. Configured states may be absent from the data.
#' @param allowedStatesList Named list of allowed destinations for each source
#'   state. Use one unique, nonempty, non-missing source name per entry, such
#'   as list(A = c("B", "C")). An empty list imposes no restrictions.
#' @param oocFix Discrete gap rule. Use "None" or "OUT OF COHORT" to leave gaps
#'   unresolved, "Last present state" to carry an allowed state forward, or
#'   a state label as a fallback. See Details for label requirements.
#' @param outOfCohortAllowed Whether retained records may extend the target
#'   interval's end. The target start is unchanged.
#' @param runSavedStudy Whether to use restored saved-study settings. Import
#'   fresh data first. Saved runs do not accept analysis-setting overrides.
#' @param useCDM Whether to join age and gender from the supplied CDM. Otherwise,
#'   age and gender_concept_id contain NA. No birth date is assumed or added.
#' @param pathToStudy Directory containing the study folder. Generation uses
#'   the location stored in studyEnv.
#' @param batchSize Positive whole-number maximum patients per batch.
#' @param mergedStateOverrides List of mergedState() definitions that replace
#'   selected inherited properties of merged states.
#'
#' @return A data frame of trajectory rows, also saved as a CSV in the study's
#'   Data directory. See the output vignette for the complete schema.
#'
#' @details
#' In discrete mode, First selects the earliest-starting eligible interval in
#' a cycle. Overlap selects the state covering the most distinct days in that
#' cycle, combining intervals without double-counting shared days. Both methods
#' use the priority order to resolve ties. Priority selects the highest-ranked
#' eligible state present during any part of the cycle. Every clinical state
#' in the bounded input must appear in the priority order.
#'
#' In continuous mode, First and Overlap both retain intervals that pass the
#' transition rules, including overlaps. Priority assigns shared days to the
#' highest-ranked eligible state and can split lower-priority intervals. It
#' also requires every clinical state to appear in the priority order.
#' Uninterrupted selected intervals do not require self-transitions at unrelated
#' record boundaries. Gaps are not filled and simultaneous records keep their
#' dates. Models requiring unique event times need an explicit tie-handling rule.
#'
#' Transition rules use the last retained clinical state. Rejected records do
#' not change that state. Source names must use the exact clinical labels.
#' Destination matching is case-insensitive. In a nonempty rule list, each
#' source used for a transition needs an entry. For merged studies, a nonempty
#' list must include every original state, including absorbing states.
#' character(0) permits no destinations. The first clinical state and boundary
#' markers need no incoming
#' rule. Only an allowed, retained absorbing state can end follow-up before
#' the observation boundary.
#'
#' For discrete gaps, carry-forward requires an allowed self-transition and a
#' named fallback must be an allowed destination. Otherwise the cycle remains
#' OUT OF COHORT. This means no selected state, not loss of observation, and
#' does not reset the last retained state for later transition checks.
#' Supply one nonempty oocFix string. "None" and "Last present state" always
#' select gap-handling behavior, even if a clinical state has that label.
#' Boundary labels "0", "START" and "EXIT" are not valid fallbacks.
#' Use the exact spelling of an existing label. A new label must not differ
#' only in case from an existing clinical or merged-state label. For merged
#' states, use the display label rather than state_key. Declare key-like text
#' in stateCohortLabels if it is intended as a separate literal state.
#'
#' @section Boundaries and timing:
#' The first target interval, labelled 0 in cleaned data, supplies the start
#' and end boundaries. Patients without clinical state records remain eligible
#' unless mandatory-state requirements exclude them. Discrete cycles include
#' both endpoint dates, and the final cycle is shortened at the observation
#' boundary. A selected absorbing state stops generation after its cycle.
#' The cycle is not independently shortened to that state's original onset date.
#'
#' Continuous trajectories end at the retained absorbing interval's end date.
#' Earlier overlapping intervals are capped at that endpoint. START and EXIT
#' are boundary markers and do not add observed time. EXIT is on the day after
#' the last discrete cycle or continuous follow-up endpoint. Its time_in_cohort
#' is elapsed years from trajectory start, using 365.25 days per year.
#'
#' Setting outOfCohortAllowed = TRUE can extend the endpoint using retained
#' state records, but cannot recover records removed during import. Public
#' imports require a target row for each included patient. The package does
#' not infer observation coverage, episode duration or clinical
#' history. An analyst-defined history-specific terminal state preserves its
#' category when selected, but a discrete row does not retain exact event dates
#' or ordering within its cycle.
#'
#' @section Study workflow:
#' Run getDataForStudy successfully before generation. An unsuccessful import
#' blocks generation until a later import succeeds. Existing output files are
#' preserved but are not reused for that failed attempt. The trajectory CSV
#' is written only after all patient batches finish, so a batch error does not
#' replace a previous complete file with a partial result.
#'
#' A completed fresh run saves Settings/analysisSettings.rds in version 4
#' format. This preserves analysis settings and any imported CDM definition
#' contents, not patient data. NULL, character(0) and nested lists retain their
#' R types. Only version 4 snapshots are accepted. Saved runs restore their
#' analysis rules and require fresh input. Configure a new study to change
#' those rules.
#'
#' @section Labels and identifiers:
#' State IDs are assigned once per study. In unmerged studies, START has ID 1,
#' followed by configured states in their supplied order, additional input and
#' discrete gap labels in alphabetical order, then EXIT. Configured states keep
#' their IDs when absent from a batch. Merged studies use dictionary order and
#' add state_key to the output. Their stateDictionary is available in studyEnv
#' and Data/stateDictionary.rds. Use keys and membership for cross-site comparison
#' rather than run-local state IDs.
#' Original-state rules remain unchanged in the configuration.
#'
#' Patient IDs are mapped internally and restored before demographic joins and
#' output. File and CDM input preserve large identifiers and leading zeros as
#' text. Canonical numeric IDs below 2^53 retain numeric output. Supply larger
#' in-memory IDs as character or integer64 because doubles may already have
#' rounded them. When reading exported CSVs, read subject_id as character to
#' preserve long or zero-prefixed identifiers. Temporary IDs are not exported.
#'
#' Working labels and rules are converted to UTF-8 without changing spelling
#' or case. Configuration and saved settings retain the supplied labels.
#'
#' @seealso \code{vignette("a03_trajectories", package = "Cohort2Trajectory")},
#'   \code{vignette("a05_output", package = "Cohort2Trajectory")}
#' @export
#' @examples
#' \dontrun{
#' getDataForStudy(cdm = cdm, studyEnv = studyEnv)
#' trajectories <- createTrajectories(cdm = cdm, studyEnv = studyEnv)
#' }
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
                               mergedStateOverrides = studyEnv$mergedStateOverrides) {
  if (isTRUE(runSavedStudy)) {
    .checkSavedOverrides(match.call(), .studyGenerationFields())
    if (is.null(studyEnv$.savedSettings) || !isTRUE(studyEnv$.dataReady)) {
      stop("Configure the saved study and run getDataForStudy successfully first. ",
           "Copied patient files are not reused for a saved run.", call. = FALSE)
    }
  }
  if (identical(studyEnv$.dataReady, FALSE)) {
    stop("The latest import did not complete successfully. Run getDataForStudy ",
         "successfully before creating trajectories. Previous patient files ",
         "will not be reused.", call. = FALSE)
  }
  trajectoryType <- .configurationChoice(
    trajectoryType, c("Discrete", "Continuous"), "trajectoryType",
    allowNull = FALSE
  )
  if (trajectoryType == "Discrete") {
    .validatePositiveInteger(lengthOfStay, "lengthOfStay", "days")
  }
  .validatePositiveInteger(batchSize, "batchSize", "patients")
  stateSelectionType <- .configurationChoice(
    stateSelectionType, c("First", "Overlap", "Priority"), "stateSelectionType",
    allowNull = FALSE
  )
  if (trajectoryType == "Discrete") .validateGapFallback(oocFix, stateCohortLabels)
  .validateMergeOptions(isTRUE(studyEnv$mergeStates), mergedStateOverrides)
  .validateStateLabels(stateCohortLabels)
  .validateAllowedStateSources(allowedStatesList)
  # Eligibility was applied before state merging. Generation cannot safely
  # change it or recover patients excluded from the cleaned input.
  imported <- studyEnv$.importSettings
  if (is.null(imported)) {
    if (length(stateCohortMandatory) || length(studyEnv$stateCohortMandatory)) {
      stop("stateCohortMandatory requires a successful getDataForStudy import ",
           "with the desired requirements before creating trajectories.", call. = FALSE)
    }
  } else if (!setequal(stateCohortMandatory, imported$stateCohortMandatory)) {
    stop("stateCohortMandatory differs from the requirements used during import. ",
         "Rerun getDataForStudy with the desired stateCohortMandatory value ",
         "before creating trajectories.", call. = FALSE)
  }
  # Save validated choices before data-dependent merged-state expansion so a
  # new site can apply the same expansion to its own observed states.
  generationSettings <- mget(.studyGenerationFields(), envir = environment(), inherits = FALSE)
  
  studyName = studyEnv$studyName
  pathToData = paste0(studyEnv$pathToStudy, '/', studyName, '/', 'Data')
  pathToStudy = studyEnv$pathToStudy
  atlasTarget = studyEnv$atlasTargetCohort
  
  data <- .readPatientCsv(
    paste0(
      pathToData,
      "/",
      "importedDataCleaned_",
      atlasTarget,
      ".csv"
    )
  )
  
  data$time_in_cohort = as.integer(data$time_in_cohort)
  data$cohort_start_date = as.Date(data$cohort_start_date)
  data$cohort_end_date = as.Date(data$cohort_end_date)
  data <- data %>% dplyr::arrange(subject_id, cohort_start_date, cohort_end_date)
  
  .validateObservedStateLabels(data$cohort_definition_id,
    if (!isTRUE(studyEnv$mergeStates)) stateCohortLabels else NULL)
  configuredStates <- as.character(stateCohortLabels)
  if (!isTRUE(studyEnv$mergeStates)) {
    knownStates <- c(configuredStates, setdiff(data$cohort_definition_id, "0"))
    if (trajectoryType == "Discrete") {
      .validateGapFallback(oocFix, knownStates)
      if (!oocFix %in% c("None", "Last present state", "OUT OF COHORT")) {
        knownStates <- c(knownStates, oocFix)
      }
    }
    .validateAbsorbingStateReferences(stateCohortAbsorbing, knownStates)
  }
  registry <- NULL
  if (isTRUE(studyEnv$mergeStates)) {
    if (is.null(studyEnv$stateDictionary)) {
      stop("Run getDataForStudy first to construct structured state membership.", call. = FALSE)
    }
    resolved <- .mergedRules(studyEnv$stateDictionary, stateCohortPriorityOrder,
                             allowedStatesList, stateCohortAbsorbing,
                             mergedStateOverrides)
    registry <- resolved$registry
    if (trajectoryType == "Discrete") .validateGapFallback(oocFix, registry$label)
    unknown <- setdiff(data$cohort_definition_id, c("0", registry$state_key))
    if (length(unknown)) stop("Cleaned data and state dictionary do not match. Rerun import.", call. = FALSE)
    stateCohortPriorityOrder <- resolved$priority
    allowedStatesList <- resolved$allowed
    stateCohortAbsorbing <- resolved$absorbing
    configuredStates <- registry$state_key
    # Gap controls are not clinical labels, even if a state shares their name.
    if (trajectoryType == "Discrete" && !oocFix %in% c("None", "Last present state")) {
      fallback <- match(oocFix, registry$label)
      if (!is.na(fallback)) {
        oocFix <- registry$state_key[fallback]
      } else {
        # An undeclared literal must not accidentally acquire another state's
        # identity, transition rules or absorbing behaviour through its key.
        key <- match(oocFix, registry$state_key)
        if (!is.na(key)) {
          stop("oocFix ", shQuote(oocFix), " is an internal state key for ",
               shQuote(registry$label[key]), ". Use that display label to select the state. ",
               "If the text is intended as a separate literal state, declare it in ",
               "stateCohortLabels and its state rules.", call. = FALSE)
        }
      }
    }
  }

  
  # Define IDs once for the whole study, before splitting patients into batches.
  # Keep configured states even when absent, and append additional labels in a
  # deterministic order (for example, labels created by state merging).
  gapState <- if (trajectoryType == "Discrete") {
    if (oocFix %in% c("None", "Last present state")) "OUT OF COHORT" else c("OUT OF COHORT", oocFix)
  } else {
    character(0)
  }
  additionalStates <- sort(setdiff(
    c(as.character(data$cohort_definition_id), gapState),
    c("0", "START", "EXIT", configuredStates)
  ), method = "radix")
  stateLevels <- c("START", configuredStates, additionalStates, "EXIT")

  # generation paramaters
  unique_subject_ids <- unique(data$subject_id)
  #batch_size <- 1000
  batches <- split(unique_subject_ids, ceiling(seq_along(unique_subject_ids) / batchSize))
  cli::cli_progress_bar("Generating trajectories ...", type = "iterator", total = length(batches))
  
  # Create an empty dataframe to store the combined results
  result <- data.frame()
  
  if (nrow(data) == 0) {
    cli::cli_abort("{.error No trajectories generated: the cleaned input is empty.}")
    return(NULL)
  }
  
  
  # Creating trajectories
  # Numeric selection codes are only used by the internal trajectory engines.
  selectionCode <- unname(c(First = 1, Overlap = 2, Priority = 3)[stateSelectionType])
  if (trajectoryType == "Discrete") {
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
          stateSelection = selectionCode,
          statePriorityVector = stateCohortPriorityOrder,
          absorbingStates = stateCohortAbsorbing,
          studyName = studyName,
          addPersonalData = useCDM,
          allowedStatesList = allowedStatesList,
          stateLevels = stateLevels,
          outOfCohortAllowed = outOfCohortAllowed
        )
      )
      
      if (nrow(result) == 0) {
        cli::cli_abort("{.error No trajectories generated as cohorts' do not increment any trajectory worthy data!}")
        return(NULL)
      }
    }
    
  }
  else if (trajectoryType == "Continuous") {
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
          stateSelection = selectionCode,
          statePriorityVector = stateCohortPriorityOrder,
          absorbingStates = stateCohortAbsorbing,
          studyName = studyName,
          addPersonalData = useCDM,
          allowedStatesList = allowedStatesList,
          stateLevels = stateLevels,
          outOfCohortAllowed = outOfCohortAllowed
        )
      )
      
      if (nrow(result) == 0) {
        cli::cli_abort("{.error No trajectories generated as cohorts' do not increment any trajectory worthy data!}")
        return(NULL)
      }
    }

  }

  if (!is.null(registry)) {
    result$state_key <- result$state_label
    matched <- match(result$state_label, registry$state_key)
    result$state_label[!is.na(matched)] <- registry$label[matched[!is.na(matched)]]
    registry$state_id <- match(registry$state_key, stateLevels)
    studyEnv$stateDictionary <- registry
  }
  save_object(result, path = file.path(pathToData, paste0("patientData", trajectoryType, ".csv")))
  if (!is.null(registry)) saveRDS(registry, file.path(pathToData, "stateDictionary.rds"))
  cli::cli_alert_info(paste0("Saved trajectory dataframe: ",
                            file.path(pathToData, paste0("patientData", trajectoryType, ".csv"))))
  cli::cli_progress_done()
  
  # Saving trajectories
  if (useCDM) {
    cli::cli_alert_info("Saving trajectories to the specified temp schema ...")
    
    cdm <- omopgenerics::insertTable(cdm, "test", result, temporary = T)
    cli::cli_alert_info("Trajectories saved to the specified temp schema!")

  }
  
  if (!runSavedStudy) {
    .saveStudySnapshot(studyEnv, generationSettings)
    cli::cli_alert_info(paste0(
      "Saved settings to: ",
      file.path(pathToStudy, studyName, "Settings", "analysisSettings.rds")
    ))
  }
  
  cli::cli_alert_success("Trajectories generated!")
  
  return(result)
}
