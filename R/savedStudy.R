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
  if (!is.list(saved) || !identical(saved$version, 1L) ||
      !is.list(saved$import) || !is.list(saved$generation) ||
      !all(.studyImportFields() %in% names(saved$import)) ||
      !all(.studyGenerationFields() %in% names(saved$generation))) {
    stop("Unsupported or incomplete saved-study settings: ", path, call. = FALSE)
  }
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
  snapshot <- list(version = 1L, import = imported, generation = generation,
                   definitions = studyEnv$.cohortDefinitions)
  saveRDS(snapshot, file.path(studyEnv$pathToStudy, studyEnv$studyName,
                             "Settings", "analysisSettings.rds"))
}
