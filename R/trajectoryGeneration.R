#' Generate patient trajectories from imported cohort intervals
#'
#' Apply the configured selection, transition and gap rules to a successful
#' study import. See \code{\link{cohort2TrajectoryConfiguration}} for these rules
#' and the required state definitions.
#'
#' @inheritParams cohort2TrajectoryConfiguration
#' @param cdm A CDM reference created with CDMConnector, required when joining
#'   demographics with \code{useCDM = TRUE}.
#' @param studyEnv Study environment returned by
#'   \code{\link{cohort2TrajectoryConfiguration}}. Its settings supply the defaults.
#' @param trajectoryType \code{"Discrete"} or \code{"Continuous"}, ignoring case
#'   and surrounding whitespace. A choice is required for generation.
#' @param stateSelectionType \code{"First"}, \code{"Overlap"} or
#'   \code{"Priority"}, ignoring case and surrounding whitespace. A choice is
#'   required for generation.
#' @param stateCohortMandatory Mandatory states used by the successful import.
#'   Rerun \code{\link{getDataForStudy}} to change them. Order and duplicates do
#'   not matter. \code{NULL} and \code{character(0)} both mean no requirements.
#' @param oocFix Discrete gap rule: \code{"None"}, \code{"OUT OF COHORT"},
#'   \code{"Last present state"} or a fallback state label. See
#'   \code{\link{cohort2TrajectoryConfiguration}} for transition and label rules.
#' @param runSavedStudy Whether to use restored saved-study settings. Import
#'   fresh data first. Saved runs do not accept analysis-setting overrides.
#' @param useCDM Whether to join age and gender from the supplied CDM. Otherwise,
#'   \code{age} and \code{gender_concept_id} contain \code{NA}.
#' @param pathToStudy Directory containing the study folder. Generation uses
#'   the location stored in \code{studyEnv}.
#'
#' @return A data frame of class \code{c2t_trajectories}, also saved as a CSV in
#'   the study's \code{Data} directory. Printing shows counts and a short preview;
#'   use \code{as.data.frame(result)} for the ordinary data-frame display. See
#'   \code{\link{print.c2t_trajectories}} for printing options and the output
#'   vignette for the complete schema.
#'   The \code{trajectory_definition} attribute records the effective trajectory
#'   type, cycle length, ordered clinical labels and absorbing labels.
#'   \pkg{TrajectoryMarkovAnalysis} reads first-period definitions directly.
#'   All-period output uses version 2 metadata with episode identity and time
#'   origin, requiring an episode-aware downstream reader. Labels and
#'   absorbing status reflect merged-state overrides; configured but unobserved
#'   states remain declared.
#'   \code{saveRDS()} preserves this attribute; CSV output contains only rows
#'   and columns.
#'
#' @section Boundaries and timing:
#' Target intervals, labelled \code{"0"} in cleaned data, supply the start and
#' end boundaries. The import's \code{targetEpisodeHandling} selects the first
#' interval or all episodes. Patients without clinical state records remain
#' eligible unless mandatory-state requirements exclude them. Discrete cycles include
#' both endpoint dates, and the final cycle is shortened at the observation
#' boundary. A selected absorbing state stops generation after its cycle.
#' Its original onset date does not shorten that cycle.
#'
#' Continuous trajectories end at the retained absorbing interval's end date.
#' Earlier overlapping intervals are capped at that endpoint. \code{START} and
#' \code{EXIT} are boundary markers and add no observed time. \code{EXIT} is on
#' the day after the last cycle or continuous endpoint. Its \code{time_in_cohort}
#' is elapsed years from trajectory start, using 365.25 days per year.
#'
#' \code{outOfCohortAllowed = TRUE} can extend the endpoint using retained state
#' records; it cannot recover records removed during import. A discrete state
#' label represents the selected state for a cycle, without preserving exact
#' event dates or ordering within that cycle.
#'
#' @section Study workflow:
#' Run \code{\link{getDataForStudy}} successfully before generation. A failed
#' import blocks generation until a later import succeeds; previous patient
#' files are preserved and are not reused for the failed attempt. The trajectory
#' CSV is written after all patient batches finish, preserving a previous
#' complete file if a batch fails. Unknown transition destinations are rejected
#' before generation writes output files.
#'
#' A completed fresh run saves \code{Settings/analysisSettings.rds} in version 6
#' format. Versions 4 and 5 also load with first-period behavior. See
#' \code{\link{cohort2TrajectoryConfiguration}} for saving and sharing rules.
#'
#' @section Labels and identifiers:
#' State IDs are assigned once per study. In unmerged studies, \code{START} has
#' ID 1, followed by configured states in their supplied order, additional input
#' and discrete gap labels in alphabetical order, then \code{EXIT}. Configured
#' states keep their IDs when absent from a batch. Merged studies use dictionary
#' order and add \code{state_key} to the output. Their \code{stateDictionary} is available
#' in \code{studyEnv} and \code{Data/stateDictionary.rds}. Use keys and membership
#' for comparison between sites; state IDs are local to the study.
#'
#' Patient identifiers are preserved. File and CDM input preserve large IDs and
#' leading zeros as text. Canonical numeric IDs below \code{2^53} retain numeric
#' output. Read \code{subject_id} as character when importing exported CSVs to
#' preserve long or zero-prefixed identifiers.
#'
#' @seealso \code{\link{cohort2TrajectoryConfiguration}},
#'   \code{vignette("a03_trajectories", package = "Cohort2Trajectory")},
#'   \code{vignette("a05_output", package = "Cohort2Trajectory")}
#' @export
#' @inherit cohort2TrajectoryConfiguration examples
createTrajectories <- function(cdm = NULL,
                               studyEnv = NULL,
                               trajectoryType = studyEnv$trajectoryType,
                               runSavedStudy = studyEnv$runSavedStudy,
                               oocFix = studyEnv$oocFix,
                               outOfCohortAllowed = studyEnv$outOfCohortAllowed,
                               cycleLength = studyEnv$cycleLength,
                               stateCohortLabels = studyEnv$stateCohortLabels,
                               stateCohortPriorityOrder = studyEnv$stateCohortPriorityOrder,
                               stateSelectionType = studyEnv$stateSelectionType,
                               stateCohortAbsorbing = studyEnv$stateCohortAbsorbing,
                               stateCohortMandatory = studyEnv$stateCohortMandatory,
                               allowedStatesList = studyEnv$allowedStatesList,
                               useCDM = studyEnv$useCDM,
                               pathToStudy = studyEnv$pathToStudy,
                               batchSize = studyEnv$batchSize,
                               mergedStateOverrides = studyEnv$mergedStateOverrides,
                               stopAfterAbsorbingEpisode = studyEnv$stopAfterAbsorbingEpisode) {
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
    .validatePositiveInteger(cycleLength, "cycleLength", "days")
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
  targetEpisodeHandling <- studyEnv$targetEpisodeHandling
  if (is.null(targetEpisodeHandling)) targetEpisodeHandling <- "First"
  targetEpisodeHandling <- .validateEpisodeOptions(
    targetEpisodeHandling, outOfCohortAllowed, stopAfterAbsorbingEpisode
  )
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
  importedPolicy <- if (is.null(imported$targetEpisodeHandling)) "First" else imported$targetEpisodeHandling
  if (!identical(targetEpisodeHandling, importedPolicy)) {
    stop("targetEpisodeHandling differs from the policy used during import. ",
         "Rerun getDataForStudy before creating trajectories.", call. = FALSE)
  }
  # Save validated choices before data-dependent merged-state expansion so a
  # new site can apply the same expansion to its own observed states.
  generationSettings <- mget(.studyGenerationFields(), envir = environment(), inherits = FALSE)
  
  studyName = studyEnv$studyName
  pathToData = paste0(studyEnv$pathToStudy, '/', studyName, '/', 'Data')
  pathToStudy = studyEnv$pathToStudy
  atlasTarget = studyEnv$atlasTargetCohort
  
  data <- .readPatientCsv(.cleanedDataPath(pathToStudy, studyName))
  if ((targetEpisodeHandling == "All") != ("episode_id" %in% names(data))) {
    stop("Cleaned data do not match targetEpisodeHandling. Rerun getDataForStudy.",
         call. = FALSE)
  }

  data$time_in_cohort = as.integer(data$time_in_cohort)
  data$cohort_start_date = as.Date(data$cohort_start_date)
  data$cohort_end_date = as.Date(data$cohort_end_date)
  data <- data %>% dplyr::arrange(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date)
  
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
    .validateAllowedStateDestinations(allowedStatesList, knownStates)
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
          stateDuration = cycleLength,
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

  if (targetEpisodeHandling == "All") {
    if (stopAfterAbsorbingEpisode) {
      result <- .omitLaterAbsorbingEpisodes(result, stateCohortAbsorbing)
    }
    result <- result[order(.subjectKeys(result$subject_id), result$episode_id,
                           result$state_start_date, seq_len(nrow(result))), , drop = FALSE]
    rownames(result) <- NULL
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
  
  if (!runSavedStudy) {
    .saveStudySnapshot(studyEnv, generationSettings)
    cli::cli_alert_info(paste0(
      "Saved settings to: ",
      file.path(pathToStudy, studyName, "Settings", "analysisSettings.rds")
    ))
  }
  
  cli::cli_alert_success("Trajectories generated!")
  
  # Attach the in-memory handoff only after writing unchanged output files.
  attr(result, "trajectory_definition") <- .trajectoryDefinition(
    trajectoryType, cycleLength, stateLevels, stateCohortAbsorbing, registry,
    targetEpisodeHandling, stopAfterAbsorbingEpisode
  )
  class(result) <- c("c2t_trajectories", class(result))
  return(result)
}

# Only states retained by the generation engine can suppress later episodes.
.omitLaterAbsorbingEpisodes <- function(data, absorbingStates) {
  selected <- data$state_label %in% absorbingStates
  if (!any(selected)) return(data)
  subjects <- .subjectKeys(data$subject_id)
  cutoff <- tapply(data$episode_id[selected], subjects[selected], min)
  limit <- unname(cutoff[match(subjects, names(cutoff))])
  data[is.na(limit) | data$episode_id <= limit, , drop = FALSE]
}
