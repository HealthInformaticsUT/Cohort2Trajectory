test_that("explicit local inputs override configured sources in ordinary and saved studies", {
  path <- tempfile("c2t-input-precedence-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  configured <- data.frame(
    subject_id = "configured_object", cohort_definition_id = "0",
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-07")
  )
  configuredCsv <- file.path(path, "configured.csv")
  overrideCsv <- file.path(path, "override.csv")
  write.csv(transform(configured, subject_id = "configured_csv"), configuredCsv, row.names = FALSE)
  write.csv(transform(configured, subject_id = "override_csv"), overrideCsv, row.names = FALSE)
  replacement <- transform(configured, subject_id = "override_object")
  original <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "inputs", useCDM = FALSE,
    atlasTargetCohort = "0", trajectoryDataObject = configured,
    pathToData = configuredCsv, stateSelectionType = "Priority", lengthOfStay = 7
  )
  getDataForStudy(studyEnv = original)
  createTrajectories(studyEnv = original)
  restored <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "inputs", useCDM = FALSE, runSavedStudy = TRUE,
    trajectoryDataObject = configured, pathToData = configuredCsv
  )
  for (study in list(original, restored)) {
    importedIds <- function(...) {
      getDataForStudy(studyEnv = study, ...)
      unique(read.csv(file.path(path, "inputs", "Data", "importedDataCleaned_0.csv"))$subject_id)
    }
    expect_identical(importedIds(), "configured_object")
    expect_identical(importedIds(pathToData = overrideCsv), "override_csv")
    expect_identical(importedIds(trajectoryDataObject = replacement), "override_object")
    expect_error(importedIds(trajectoryDataObject = replacement, pathToData = overrideCsv),
                   "only one.*trajectoryDataObject.*pathToData")
    expect_identical(importedIds(trajectoryDataObject = NULL), "configured_csv")
    expect_identical(study$trajectoryDataObject, configured)
    expect_identical(study$pathToData, configuredCsv)
  }
  # Local arguments must not take precedence over an explicitly selected CDM.
  local_mocked_bindings(getCohortData = function(...) stop("CDM selected"))
  expect_error(getDataForStudy(
    studyEnv = original, useCDM = TRUE,
    trajectoryDataObject = replacement, pathToData = overrideCsv
  ), "CDM selected")
})
