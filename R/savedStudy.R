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
         ". Configure a study and complete getDataForStudy and createTrajectories ",
         "successfully, then copy its Settings folder to the new location.",
         call. = FALSE)
  }
  saved <- readRDS(path)
  if (!is.list(saved) || !identical(saved$version, 4L)) {
    stop("Unsupported saved-study format at ", path,
         ". Only version 4 is supported. Configure the study with runSavedStudy = FALSE ",
         "and complete import and trajectory generation with this package version.",
         call. = FALSE)
  }
  if (!is.list(saved$import) || !is.list(saved$generation) ||
      !all(.studyImportFields() %in% names(saved$import)) ||
      !all(.studyGenerationFields() %in% names(saved$generation))) {
    stop("Unsupported or incomplete saved-study settings: ", path, call. = FALSE)
  }
  if (!setequal(saved$import$stateCohortMandatory, saved$generation$stateCohortMandatory)) {
    stop("Saved stateCohortMandatory requirements disagree between import and generation. ",
         "Configure a new study with the intended requirements, then rerun ",
         "getDataForStudy and createTrajectories. The saved file has not been changed.",
         call. = FALSE)
  }
  # Validate the current snapshot without rewriting the user's file.
  .validateStateLabels(saved$import$stateCohortLabels)
  .validateStateLabels(saved$generation$stateCohortLabels)
  .validateAllowedStateSources(saved$import$allowedStatesList, "Saved import allowedStatesList")
  .validateAllowedStateSources(saved$generation$allowedStatesList, "Saved generation allowedStatesList")
  saved$generation$trajectoryType <- .configurationChoice(
    saved$generation$trajectoryType, c("Discrete", "Continuous"),
    "trajectoryType", allowNull = FALSE
  )
  if (saved$generation$trajectoryType == "Discrete") {
    .validateGapFallback(saved$generation$oocFix,
                         saved$generation$stateCohortLabels)
  }
  saved$generation$stateSelectionType <- .configurationChoice(
    saved$generation$stateSelectionType, c("First", "Overlap", "Priority"),
    "stateSelectionType", allowNull = FALSE
  )
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
  # Nested local definitions are valid manifest entries. Keep their relative
  # paths, but reject absolute paths and parent traversal on either platform.
  comparisonPaths <- gsub("\\\\", "/", ids)
  if (anyNA(ids) || any(!nzchar(ids)) ||
      any(grepl("^(/|[A-Za-z]:)|(^|/)\\.\\.(/|$)", comparisonPaths))) {
    stop("Saved cohort definitions must use relative paths within the JSON folder, without parent traversal.",
         call. = FALSE)
  }
  contents <- saved$definitionContents
  if (is.null(contents)) {
    stop("These saved CDM settings do not contain the cohort definitions used by the completed run. ",
         "Configure the original study with runSavedStudy = FALSE and rerun import and generation ",
         "to save self-contained settings.", call. = FALSE)
  }
  if (!is.character(contents) || is.null(names(contents)) || anyNA(names(contents)) ||
      anyDuplicated(names(contents)) || !setequal(names(contents), unique(ids)) ||
      anyNA(contents) || any(!nzchar(trimws(contents)))) {
    stop("Saved cohort-definition contents are incomplete or inconsistent with their mapping. ",
         "Rerun the original configured import and generation.", call. = FALSE)
  }
  invisible(NULL)
}

# Capture the actual definitions read by the import, not the mutable working
# JSON files that might have changed by the time generation completes.
.readDefinitionContents <- function(definitions, jsonPath) {
  ids <- unique(c(definitions$target, definitions$states))
  files <- file.path(jsonPath, paste0(ids, ".json"))
  stats::setNames(vapply(files, function(path) {
    readChar(path, nchars = file.info(path)$size, useBytes = TRUE)
  }, character(1)), ids)
}

.stageSavedDefinitions <- function(saved, jsonPath) {
  .checkSavedDefinitions(saved, NULL, NULL)
  # Only called with a newly created private directory. No working-tree
  # symlinks or edited definitions can influence the restored run.
  dir.create(jsonPath, recursive = TRUE, showWarnings = FALSE)
  for (id in names(saved$definitionContents)) {
    path <- file.path(jsonPath, paste0(id, ".json"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeChar(saved$definitionContents[[id]], path, eos = NULL, useBytes = TRUE)
  }
  invisible(NULL)
}

.saveStudySnapshot <- function(studyEnv, generation) {
  imported <- studyEnv$.importSettings
  if (is.null(imported)) {
    imported <- mget(.studyImportFields(), envir = studyEnv, inherits = FALSE)
  }
  snapshot <- list(version = 4L, import = imported, generation = generation,
                   definitions = studyEnv$.cohortDefinitions,
                   definitionContents = studyEnv$.cohortDefinitionContents,
                   stateDictionary = studyEnv$stateDictionary)
  if (!is.null(snapshot$definitions)) .checkSavedDefinitions(snapshot, NULL, NULL)
  settingsPath <- file.path(studyEnv$pathToStudy, studyEnv$studyName, "Settings")
  staged <- tempfile(".analysisSettings-", tmpdir = settingsPath)
  on.exit(unlink(staged), add = TRUE)
  saveRDS(snapshot, staged)
  if (!file.rename(staged, file.path(settingsPath, "analysisSettings.rds"))) {
    stop("Could not publish the completed study settings. The previous snapshot has been retained.", call. = FALSE)
  }
}
