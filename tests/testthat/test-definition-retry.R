test_that("failed CDM imports preserve the definitions used by saved studies", {
  root <- tempfile("c2t-definition-retry-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  failure <- NULL
  seenTarget <- NULL
  local_mocked_bindings(
    getCohortDefinition = function(cohortId, baseUrl) {
      if (identical(failure, "download") && cohortId == 404) stop("Download failed")
      list(expression = list(definition = as.character(cohortId)))
    }, .package = "ROhdsiWebApi"
  )
  local_mocked_bindings(
    readCohortSet = function(path) {
      seenTarget <<- RJSONIO::fromJSON(file.path(path, "0.json"), simplify = FALSE)$definition
      list()
    },
    generateCohortSet = function(cdm, cohortSet, name) {
      if (identical(failure, "query")) stop("Query failed")
      cdm
    }, .package = "CDMConnector"
  )
  raw <- data.frame(
    cohort_definition_id = c(1L, 2L), subject_id = "old_patient",
    cohort_start_date = as.Date("2020-01-01"), cohort_end_date = as.Date("2020-01-07")
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, studyName = "replay", atlasTargetCohort = 101,
    atlasStateCohorts = 202, stateCohortLabels = "A", stateCohortPriorityOrder = "A",
    stateSelectionType = "Priority", lengthOfStay = 7
  )
  getDataForStudy(cdm = list(cohort = raw), studyEnv = study)
  createTrajectories(studyEnv = study, useCDM = FALSE)
  json <- file.path(root, "replay", "JSON")
  writeLines("Unrelated local definition", file.path(json, "unrelated.json"))
  files <- list.files(file.path(root, "replay"), full.names = TRUE, recursive = TRUE)
  before <- tools::md5sum(files)
  for (failure in c("download", "query", "empty")) {
    attempt <- function() getDataForStudy(
      cdm = list(cohort = if (failure == "empty") raw[2, , drop = FALSE] else raw),
      studyEnv = study, atlasTargetCohort = 303, atlasStateCohorts = 404
    )
    if (failure == "empty") {
      expect_warning(attempt(), "No patients left after cleaning")
    } else {
      expect_error(attempt(), "failed")
    }
    expect_false(study$.dataReady)
    expect_identical(list.files(file.path(root, "replay"), full.names = TRUE, recursive = TRUE), files)
    expect_identical(tools::md5sum(files), before)
  }
  failure <- NULL
  restored <- cohort2TrajectoryConfiguration(pathToStudy = root, studyName = "replay", runSavedStudy = TRUE)
  getDataForStudy(cdm = list(cohort = raw), studyEnv = restored)
  expect_identical(seenTarget, "101")

  raw$subject_id <- "new_patient"
  getDataForStudy(cdm = list(cohort = raw), studyEnv = study,
                  atlasTargetCohort = 303, atlasStateCohorts = 404)
  expect_identical(RJSONIO::fromJSON(file.path(json, "0.json"), simplify = FALSE)$definition, "303")
  expect_true(file.exists(file.path(json, "atlas_404.json")))
  expect_identical(readLines(file.path(json, "unrelated.json")), "Unrelated local definition")
  # A successful new import must not change the definitions of the last
  # completed study when generation fails or has not yet been attempted.
  expect_error(createTrajectories(studyEnv = study, useCDM = FALSE,
                                  stateCohortPriorityOrder = "missing"), "missing from statePriorityVector")
  restored <- cohort2TrajectoryConfiguration(pathToStudy = root, studyName = "replay", runSavedStudy = TRUE)
  getDataForStudy(cdm = list(cohort = raw), studyEnv = restored)
  expect_identical(seenTarget, "101")
  writeLines('{"definition":"edited_after_import"}', file.path(json, "0.json"))
  expect_identical(unique(createTrajectories(studyEnv = study, useCDM = FALSE)$subject_id), "new_patient")
  settingsFile <- file.path(root, "replay", "Settings", "analysisSettings.rds")
  snapshot <- readRDS(settingsFile)
  expect_identical(RJSONIO::fromJSON(snapshot$definitionContents[["0"]], simplify = FALSE)$definition, "303")
  # A failed publication cannot truncate or replace the completed snapshot.
  before <- tools::md5sum(settingsFile)
  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  expect_error(.saveStudySnapshot(study, snapshot$generation), "previous snapshot has been retained")
  expect_identical(tools::md5sum(settingsFile), before)
  expect_identical(list.files(dirname(settingsFile), all.files = TRUE, no.. = TRUE), "analysisSettings.rds")
})
