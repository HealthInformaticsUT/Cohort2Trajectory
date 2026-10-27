# Keep portable analysis rules separate from connections, paths and patient data.
.studyImportFields <- function() {
  c("stateCohortLabels", "stateCohortPriorityOrder", "stateCohortMandatory",
    "stateCohortAbsorbing", "outOfCohortAllowed", "allowedStatesList",
    "mergeStates", "mergeThreshold")
}

.studyGenerationFields <- function() {
  c("trajectoryType", "oocFix", "outOfCohortAllowed", "lengthOfStay",
    "stateCohortLabels", "stateCohortPriorityOrder", "stateSelectionType",
    "stateCohortAbsorbing", "stateCohortMandatory", "allowedStatesList",
    "updateAutomaticallyForMergedStates")
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
  if (!is.list(saved) || !(identical(saved$version, 1L) || identical(saved$version, 2L)) ||
      !is.list(saved$import) || !is.list(saved$generation) ||
      !all(.studyImportFields() %in% names(saved$import)) ||
      !all(.studyGenerationFields() %in% names(saved$generation))) {
    stop("Unsupported or incomplete saved-study settings: ", path, call. = FALSE)
  }
  # Migrate legacy snapshots in memory without rewriting the user's file or
  # warning about codes that the package itself previously saved.
  saved$generation$trajectoryType <- .configurationChoice(
    saved$generation$trajectoryType, c(Discrete = 0, Continuous = 1),
    "trajectoryType", allowNull = FALSE, warnCodes = FALSE
  )
  saved$generation$stateSelectionType <- .configurationChoice(
    saved$generation$stateSelectionType, c(First = 1, Overlap = 2, Priority = 3),
    "stateSelectionType", allowNull = FALSE, warnCodes = FALSE
  )
  saved$version <- 2L
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
  snapshot <- list(version = 2L, import = imported, generation = generation,
                   definitions = studyEnv$.cohortDefinitions)
  saveRDS(snapshot, file.path(studyEnv$pathToStudy, studyEnv$studyName,
                             "Settings", "analysisSettings.rds"))
}
