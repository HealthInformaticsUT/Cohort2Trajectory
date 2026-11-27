#' Configure a trajectory study
#'
#' Choose cohorts and rules, create the study folders and return an environment
#' for \code{\link{getDataForStudy}} and \code{\link{createTrajectories}}.
#'
#' @param cdm Not used by this function. Pass a connected CDMConnector object
#'   to \code{getDataForStudy()} and \code{createTrajectories()} when those
#'   steps use a CDM.
#' @param studyName Study name, used as the folder name.
#' @param baseUrl ATLAS WebAPI URL for cohort definitions. Use \code{NULL}
#'   to read local definitions from the study's \code{JSON} folder during import.
#' @param pathToStudy Parent folder containing the study folder.
#' @param atlasTargetCohort ATLAS ID of the target cohort. With
#'   \code{baseUrl = NULL}, supply its local JSON filename without the extension.
#'   Target intervals define each patient's observation periods.
#' @param atlasStateCohorts Named vector of ATLAS state cohort IDs, such as
#'   \code{c(Stable = 101, Complication = 102, Dead = 103)}. Names become the
#'   state labels. With \code{baseUrl = NULL}, use local JSON filenames without
#'   their extensions as the values. An unnamed vector instead requires
#'   \code{stateCohortLabels} in the same order.
#' @param stateCohortLabels Character vector of state labels. May be omitted
#'   when \code{atlasStateCohorts} is named. If both are supplied, the labels
#'   must match its names in order. Labels must be nonempty, non-missing and unique
#'   even when ignoring case. \code{"0"}, \code{"START"}, \code{"EXIT"} and
#'   \code{"OUT OF COHORT"} are reserved, including case variants. Labels may
#'   contain spaces or punctuation. They are separate from database cohort
#'   names and definition filenames.
#' @param stateCohortPriorityOrder State labels from highest to lowest priority.
#'   All states within the observation bounds must be listed for discrete
#'   selection or continuous \code{"Priority"} selection. For a merged study,
#'   \code{NULL} uses a deterministic order of original states.
#' @param stateCohortMandatory State labels that every included patient must
#'   have within the retained observation bounds. \code{NULL} or
#'   \code{character(0)} imposes no requirement. Labels are literal.
#'   Requirements are applied during import; rerun
#'   \code{\link{getDataForStudy}} to change them.
#' @param stateCohortAbsorbing State labels that end follow-up when selected.
#'   \code{NULL} or \code{character(0)} means none. For an unmerged study, use
#'   exact configured or observed labels, or a named discrete fallback.
#'   Configured states may be absent from the current data. For merged studies,
#'   supply original state labels here and use \code{mergedStateOverrides}
#'   for exceptions.
#' @param stateSelectionType \code{"First"}, \code{"Overlap"} or
#'   \code{"Priority"}. Names ignore case and surrounding whitespace.
#'   \code{NULL} is allowed during configuration, but generation needs a choice.
#' @param oocFix Discrete gap rule. \code{"None"} or \code{"OUT OF COHORT"}
#'   leaves an unresolved cycle. \code{"Last present state"} carries the last
#'   selected state forward when permitted. Any other nonempty, non-missing
#'   string names a fallback state. See the gap rules below.
#' @param trajectoryType \code{"Discrete"} or \code{"Continuous"}.
#'   Names ignore case and surrounding whitespace. \code{NULL} is allowed
#'   during configuration, but generation needs a choice.
#' @param cycleLength Positive whole-number cycle length in days, within R's
#'   integer range. Required when generating discrete trajectories. It may be
#'   \code{NULL} during configuration. The final cycle is shortened at the
#'   observation boundary.
#' @param outOfCohortAllowed Whether retained state records may extend the
#'   trajectory beyond the target interval's end. The target start is unchanged.
#' @param runSavedStudy Set \code{TRUE} to restore the completed study from
#'   \code{studyName/Settings/analysisSettings.rds}. Supply its location and
#'   fresh input or connections, without repeating analytical settings.
#' @param useCDM Whether to use a CDM for import and demographic joins during
#'   generation. Use \code{FALSE} for a local data frame or CSV.
#' @param trajectoryDataObject Data frame for local input, with columns
#'   \code{subject_id}, \code{cohort_definition_id}, \code{cohort_start_date}
#'   and \code{cohort_end_date}. Use label \code{"0"} for observation intervals
#'   and the chosen state labels for state intervals.
#' @param allowedStatesList Named list of allowed destinations, for example
#'   \code{list(A = c("B", "C"))}. Source names must be unique, nonempty and
#'   non-missing. Put all destinations for one source in a single entry.
#'   An empty list allows all transitions. \code{character(0)} or \code{NULL}
#'   in an individual entry permits no destinations.
#'   Nonmerged generation checks destinations against configured and observed
#'   states and any named discrete fallback, ignoring case. Unknown destinations
#'   are rejected before output files are written. Configured states may be
#'   unobserved.
#' @param mergeStates Set \code{TRUE} to represent overlapping state intervals
#'   as a combined state with explicit membership.
#' @param mergedStateOverrides List of \code{\link{mergedState}} definitions.
#'   Requires \code{mergeStates = TRUE}. Supply at most one definition per
#'   membership set. Omitted properties retain inherited values.
#' @param pathToData CSV path for local input when no data frame is configured.
#'   The CSV uses the same columns as \code{trajectoryDataObject}. Individual
#'   imports can select a different input through \code{getDataForStudy()}.
#' @param batchSize Maximum patients per generation batch. Supply one positive
#'   whole number within R's integer range. The default is 1000.
#' @param targetEpisodeHandling \code{"First"} (default) retains the earliest
#'   target interval per patient. \code{"All"} retains separate, nonoverlapping
#'   intervals as episodes. Names ignore case and surrounding whitespace.
#'   Requires \code{outOfCohortAllowed = FALSE}. Changing this choice requires
#'   another import.
#' @param stopAfterAbsorbingEpisode Whether a selected absorbing state also
#'   excludes later episodes for that patient. The default, \code{FALSE}, lets
#'   later episodes start afresh. An absorbing state always ends its own episode.
#'
#' @return An environment of class \code{c2t_study} containing the study
#'   settings. Printing shows a compact summary; inspect individual settings
#'   with \code{study$settingName} or all settings with
#'   \code{as.list(study, all.names = TRUE)}.
#'
#' @section Observation and state definitions:
#' The analyst defines target and state cohorts. By default, the first target
#' interval for each patient sets the observation bounds, including for
#' patients without state records. State cohorts describe events within those
#' bounds; their labels do not change their dates or clinical meaning.
#'
#' Date intervals include both endpoints. A shared boundary date counts as
#' one day of overlap. Discrete trajectories assign a state to each cycle.
#' Continuous trajectories retain selected intervals and boundary markers
#' without filling gaps or changing the times of simultaneous events.
#'
#' With \code{targetEpisodeHandling = "All"}, identical target intervals are
#' deduplicated and overlapping intervals are rejected. Adjacent intervals are
#' separate episodes. State records are clipped to every period they intersect;
#' gaps between periods produce no rows. Mandatory states are required within
#' each episode, before state merging. The chronological \code{episode_id} is
#' assigned before eligibility filtering and is retained in output alongside
#' the original \code{subject_id}. Cycles, elapsed time, sequence numbering,
#' transition checks and carry-forward restart in each episode. An absorbing
#' state ends that episode; later episodes remain eligible unless
#' \code{stopAfterAbsorbingEpisode = TRUE}.
#'
#' @section State selection and gaps:
#' In discrete trajectories, only overlapping states allowed from the last
#' selected state compete. \code{"First"} chooses the earliest start.
#' \code{"Overlap"} chooses the state covering the most distinct days in the
#' cycle, combining repeated records without counting a day twice.
#' \code{"Priority"} follows the supplied order. Ties use that order.
#' Missing priority entries raise an error for all three discrete methods.
#'
#' If no state qualifies, \code{oocFix} applies. Carry-forward needs an allowed
#' self-transition, and a named fallback must be an allowed destination.
#' Otherwise the cycle is \code{"OUT OF COHORT"}, meaning no state was selected.
#' Observation continues, and later transitions use the last selected state.
#'
#' \code{"None"} and \code{"Last present state"} always select gap behaviour,
#' even if a clinical state has that label. Fallbacks cannot be \code{"0"},
#' \code{"START"} or \code{"EXIT"}. Use the exact spelling of an existing
#' state. A new fallback must not differ only in case from an existing state.
#' For a merged state, use its display label, not its internal \code{state_key}.
#' Text resembling a key can be a separate literal state if declared in
#' \code{stateCohortLabels}.
#'
#' Continuous \code{"First"} and \code{"Overlap"} both retain allowed intervals,
#' including overlaps. \code{"Priority"} assigns overlapping days to the
#' highest-priority allowed state and can split lower-priority intervals.
#' Continuous \code{"Priority"} requires every state within the bounds in the
#' priority order. An ongoing interval does not need a self-transition when
#' another record starts or ends.
#' A selected absorbing interval keeps its bounded endpoint even if a
#' higher-priority record arrives later. Models requiring unique timestamps
#' must resolve simultaneous events.
#'
#' @section Transitions and absorbing states:
#' Source names use exact state labels. Destination matching ignores case.
#' Output preserves the clinical labels.
#' In an unmerged study with a nonempty rule list, an entry is required only
#' when a state is used as a transition source. Rules apply separately to each
#' patient and use the last retained clinical state; rejected records do not
#' change that state. The first clinical state and boundary markers need no
#' incoming rule.
#'
#' A prohibited absorbing record does not end follow-up. Discrete generation
#' ends after the selected absorbing cycle. Continuous generation ends at the
#' retained absorbing interval's endpoint. Generation does not check outgoing
#' transitions after a retained absorbing state.
#'
#' @section Combining overlapping states:
#' With \code{mergeStates = TRUE}, exact overlaps form combined states before
#' discretisation. Membership is stored explicitly. Labels are never split to
#' infer membership. A nonempty original-state rule list must include every
#' original state. Use an empty list for unrestricted transitions.
#'
#' Allowed destinations are inherited from the union of the source members'
#' destinations. Each member newly added at the destination must be allowed by
#' at least one source member. For a self-transition or a transition that only
#' removes members, every retained destination member needs that permission.
#'
#' A combination is absorbing if any member is absorbing. Its highest-priority
#' member determines its priority. At equal priority, larger combinations come
#' first, followed by state key order. Without a supplied priority order,
#' original states are ordered deterministically.
#' Explicit \code{\link{mergedState}} properties replace inherited properties.
#' Omitted properties retain inherited values. Setting \code{allowedTransitions}
#' to \code{NULL}, \code{character(0)} or \code{list()} permits no destinations.
#' Absorbing status is separate and takes precedence over outgoing transitions.
#'
#' The study environment's \code{stateDictionary} and the generated
#' \code{Data/stateDictionary.rds} record the resolved states and their rules.
#' The original-state rules remain unchanged in the configuration.
#'
#' @section Saving and sharing a study:
#' A completed import and generation saves the analysis settings in
#' \code{Settings/analysisSettings.rds} in version 6 format. Versions 4 and 5
#' load with \code{targetEpisodeHandling = "First"} and
#' \code{stopAfterAbsorbingEpisode = FALSE}. Reconfigure earlier formats. Omitted and
#' explicit empty override fields remain distinct in saved settings.
#' CDM snapshots contain the exact cohort JSON definitions captured at import.
#' Saved runs use those definitions and fresh patient data, leaving working
#' JSON files unchanged.
#'
#' Copy the completed study's \code{Settings} folder to the new location.
#' Do not share \code{Data}, which contains patient records. Supply the new
#' connection to import and generation, or configure fresh local input with
#' \code{useCDM = FALSE}. Import must succeed before generation.
#' To change definitions or analysis rules, configure a fresh study.
#'
#' Local definition names may contain relative subpaths, such as
#' \code{states/A} for \code{JSON/states/A.json}. Saved paths cannot be absolute
#' or contain parent traversal.
#'
#' @section Patient identifiers:
#' IDs may be numeric whole numbers, character or integer64 values.
#' Missing or blank identifiers are rejected. Temporary internal IDs are
#' replaced by the original identifiers before output. For in-memory IDs at or
#' above \code{2^53}, use character or integer64 input to avoid rounded doubles.
#'
#' @seealso \code{\link{getDataForStudy}}, \code{\link{createTrajectories}},
#'   \code{\link{mergedState}}
#' @export
#' @examples
#' cohorts <- data.frame(
#'   subject_id = rep("001", 3),
#'   cohort_definition_id = c("0", "Stable", "Complication"),
#'   cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7),
#'   cohort_end_date = as.Date("2020-01-21")
#' )
#' folder <- tempfile("c2t-example-")
#' study <- cohort2TrajectoryConfiguration(
#'   studyName = "example", pathToStudy = folder,
#'   useCDM = FALSE, trajectoryDataObject = cohorts,
#'   stateCohortLabels = c("Stable", "Complication"),
#'   stateCohortPriorityOrder = c("Complication", "Stable"),
#'   stateSelectionType = "Priority", cycleLength = 7
#' )
#' getDataForStudy(studyEnv = study)
#' trajectories <- createTrajectories(studyEnv = study)
#' trajectories[c("subject_id", "state_label", "state_start_date", "state_end_date")]
#' unlink(folder, recursive = TRUE)
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
                                           cycleLength = NULL,
                                           outOfCohortAllowed = FALSE,
                                           
                                           runSavedStudy = FALSE,
                                           useCDM = TRUE,
                                           trajectoryDataObject = NULL,
                                           pathToData = './Cohort2Trajectory/Data/importedData.csv',
                                           
                                           allowedStatesList = createStateList(stateCohortLabels),
                                           mergeStates = FALSE,
                                           batchSize = 1000,
                                           mergedStateOverrides = list(),
                                           targetEpisodeHandling = "First",
                                           stopAfterAbsorbingEpisode = FALSE) {
  .validateStudyPath(pathToStudy, studyName)
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
    class(studyEnv) <- c("c2t_study", "environment")
    return(studyEnv)
  }
  stateCohortLabels <- .stateLabelsFromCohorts(atlasStateCohorts, stateCohortLabels)
  atlasStateCohorts <- unname(atlasStateCohorts)
  # Validate choices before creating study files. NULL remains available for
  # workflows that supply these settings later or load a saved study.
  stateSelectionType <- .configurationChoice(
    stateSelectionType, c("First", "Overlap", "Priority"), "stateSelectionType"
  )
  trajectoryType <- .configurationChoice(
    trajectoryType, c("Discrete", "Continuous"), "trajectoryType"
  )
  if (isTRUE(trajectoryType == "Discrete") && !is.null(cycleLength)) {
    .validatePositiveInteger(cycleLength, "cycleLength", "days")
  }
  .validatePositiveInteger(batchSize, "batchSize", "patients")
  if (isTRUE(trajectoryType == "Discrete")) .validateGapFallback(oocFix, stateCohortLabels)
  .validateMergeOptions(mergeStates, mergedStateOverrides)
  .validateStateLabels(stateCohortLabels)
  .validateAllowedStateSources(allowedStatesList)
  targetEpisodeHandling <- .validateEpisodeOptions(
    targetEpisodeHandling, outOfCohortAllowed, stopAfterAbsorbingEpisode
  )
  
  if (studyName %in% list.dirs(pathToStudy, full.names = FALSE, recursive = FALSE)) {
    cli::cli_warn("{.warning Study name already in use, consider renaming!}")
  }
  createMandatorySubDirs(pathToStudy, studyName)
  
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
    targetEpisodeHandling = targetEpisodeHandling,

    stateCohortLabels = stateCohortLabels,
    stateCohortPriorityOrder = stateCohortPriorityOrder,
    stateCohortMandatory = stateCohortMandatory,
    stateCohortAbsorbing = stateCohortAbsorbing,
    stateSelectionType = stateSelectionType,

    # trajectory generation
    oocFix = oocFix,
    trajectoryType = trajectoryType,
    cycleLength = cycleLength,
    outOfCohortAllowed = outOfCohortAllowed,
    stopAfterAbsorbingEpisode = stopAfterAbsorbingEpisode,

    batchSize = batchSize
  )
  
  class(studyEnv) <- c("c2t_study", "environment")
  return(studyEnv)
}

.configurationChoice <- function(value, choices, argument, allowNull = TRUE) {
  if (is.null(value) && allowNull) return(NULL)
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    index <- match(tolower(trimws(value)), tolower(choices))
    if (!is.na(index)) return(choices[index])
  }
  stop(argument, " must be one of: ", paste(choices, collapse = ", "),
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

.validateEpisodeOptions <- function(targetEpisodeHandling, outOfCohortAllowed,
                                    stopAfterAbsorbingEpisode = FALSE) {
  policy <- .configurationChoice(targetEpisodeHandling, c("First", "All"),
                                 "targetEpisodeHandling", allowNull = FALSE)
  if (!is.logical(stopAfterAbsorbingEpisode) ||
      length(stopAfterAbsorbingEpisode) != 1L || is.na(stopAfterAbsorbingEpisode)) {
    stop("stopAfterAbsorbingEpisode must be TRUE or FALSE.", call. = FALSE)
  }
  if (policy == "All" && !identical(outOfCohortAllowed, FALSE)) {
    stop("targetEpisodeHandling = 'All' requires outOfCohortAllowed = FALSE.",
         call. = FALSE)
  }
  policy
}
