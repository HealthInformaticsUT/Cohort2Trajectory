# Keep portable analysis rules separate from connections, paths and patient data.
.cohortDefinitionFiles <- function(target, states, baseUrl) {
  if (is.null(baseUrl)) {
    return(list(target = as.character(target), states = as.character(states)))
  }
  # Match the integer IDs sent to ATLAS without using clinical labels as paths.
  # The prefix keeps a downloaded state with ID 0 separate from target 0.json.
  stateFiles <- if (length(states)) paste0("atlas_", as.integer(states)) else character(0)
  list(target = "0", states = stateFiles)
}

.studyImportFields <- function() {
  c("stateCohortLabels", "stateCohortPriorityOrder", "stateCohortMandatory",
    "stateCohortAbsorbing", "outOfCohortAllowed", "allowedStatesList",
    "mergeStates", "mergedStateOverrides")
}

.studyGenerationFields <- function() {
  c("trajectoryType", "oocFix", "outOfCohortAllowed", "lengthOfStay",
    "stateCohortLabels", "stateCohortPriorityOrder", "stateSelectionType",
    "stateCohortAbsorbing", "stateCohortMandatory", "allowedStatesList",
    "mergedStateOverrides")
}

.checkSavedOverrides <- function(call, fields) {
  supplied <- intersect(names(as.list(call)[-1L]), fields)
  if (length(supplied)) {
    stop("A saved study restores its analysis settings. Do not also supply: ",
         paste(supplied, collapse = ", "), ". Configure a new study to change its rules.",
         call. = FALSE)
  }
}

.readStudySnapshot <- function(pathToStudy, studyName) {
  path <- file.path(pathToStudy, studyName, "Settings", "analysisSettings.rds")
  if (!file.exists(path)) {
    stop("No complete saved-study settings found at ", path,
         ". Older CSV settings did not save transition or merging rules. ",
         "Rerun the original configured import and trajectory generation with this ",
         "package version once, then copy JSON and Settings to the new location.",
         call. = FALSE)
  }
  saved <- readRDS(path)
  if (!is.list(saved) || !any(vapply(1:3, function(v) identical(saved$version, v), logical(1))) ||
      !is.list(saved$import) || !is.list(saved$generation)) {
    stop("Unsupported or incomplete saved-study settings: ", path, call. = FALSE)
  }
  if (saved$version < 3L) {
    if (isTRUE(saved$import$mergeStates)) {
      stop("Legacy merged-study settings have no structured membership. Reconfigure with the original state definitions and mergedStateOverrides, then rerun import and generation. Labels will not be parsed to guess membership.", call. = FALSE)
    }
    if ("mergeThreshold" %in% names(saved$import)) {
      warning("Removed obsolete mergeThreshold from legacy saved settings. No threshold is used.", call. = FALSE)
    }
    saved$import$mergeThreshold <- NULL
    saved$generation$updateAutomaticallyForMergedStates <- NULL
    saved$import$mergedStateOverrides <- list()
    saved$generation$mergedStateOverrides <- list()
  }
  if (
      !all(.studyImportFields() %in% names(saved$import)) ||
      !all(.studyGenerationFields() %in% names(saved$generation))) {
    stop("Unsupported or incomplete saved-study settings: ", path, call. = FALSE)
  }
  # Migrate legacy snapshots in memory without rewriting the user's file or
  # warning about codes that the package itself previously saved.
  .validateStateLabels(saved$import$stateCohortLabels)
  .validateStateLabels(saved$generation$stateCohortLabels)
  saved$generation$trajectoryType <- .configurationChoice(
    saved$generation$trajectoryType, c(Discrete = 0, Continuous = 1),
    "trajectoryType", allowNull = FALSE, warnCodes = FALSE
  )
  if (saved$generation$trajectoryType == "Discrete") {
    .validateGapFallback(saved$generation$oocFix,
                         saved$generation$stateCohortLabels)
  }
  saved$generation$stateSelectionType <- .configurationChoice(
    saved$generation$stateSelectionType, c(First = 1, Overlap = 2, Priority = 3),
    "stateSelectionType", allowNull = FALSE, warnCodes = FALSE
  )
  saved$version <- 3L
  saved
}

.checkSavedDefinitions <- function(saved, pathToStudy, studyName) {
  definitions <- saved$definitions
  if (!is.list(definitions) || length(definitions$target) != 1L ||
      length(definitions$states) != length(saved$import$stateCohortLabels)) {
    stop("This saved study has no complete CDM cohort-definition mapping. ",
         "Save it after a successful CDM import, or use fresh non-CDM input.",
         call. = FALSE)
  }
  ids <- c(definitions$target, definitions$states)
  if (anyNA(ids) || any(!nzchar(ids)) || any(grepl("[/\\\\]", ids))) {
    stop("Saved cohort definitions must be file names within the JSON folder.",
         call. = FALSE)
  }
  files <- file.path(pathToStudy, studyName, "JSON", paste0(ids, ".json"))
  missing <- files[!file.exists(files) | dir.exists(files)]
  if (length(missing)) {
    stop("Missing saved cohort definition(s): ", paste(basename(missing), collapse = ", "),
         ". Copy the study's JSON folder as well as Settings.", call. = FALSE)
  }
  invisible(NULL)
}

.saveStudySnapshot <- function(studyEnv, generation) {
  imported <- studyEnv$.importSettings
  if (is.null(imported)) {
    imported <- mget(.studyImportFields(), envir = studyEnv, inherits = FALSE)
  }
  snapshot <- list(version = 3L, import = imported, generation = generation,
                   definitions = studyEnv$.cohortDefinitions,
                   stateDictionary = studyEnv$stateDictionary)
  saveRDS(snapshot, file.path(studyEnv$pathToStudy, studyEnv$studyName,
                             "Settings", "analysisSettings.rds"))
}
