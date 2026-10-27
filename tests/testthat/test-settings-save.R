test_that("optional state lists survive saving, updating and reloading settings", {
  path <- tempfile("c2t-settings-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  study <- cohort2TrajectoryConfiguration(
    studyName = "settings", pathToStudy = path, useCDM = FALSE,
    atlasTargetCohort = "0", stateCohortLabels = c("A", "B"),
    stateCohortPriorityOrder = c("A", "B"), stateSelectionType = "Priority", lengthOfStay = 7
  )
  expect_length(list.files(file.path(path, "settings", "Settings")), 0)
  data <- data.frame(subject_id = c(1L, 1L), cohort_definition_id = c("0", "A"),
                     cohort_start_date = as.Date("2020-01-01"),
                     cohort_end_date = as.Date("2020-01-07"), time_in_cohort = 0)
  utils::write.csv(data, file.path(path, "settings", "Data", "importedDataCleaned_0.csv"),
                   row.names = FALSE)
  for (states in list(NULL, c("A", "B"), character(0))) {
    createTrajectories(studyEnv = study, stateCohortAbsorbing = states,
                       stateCohortMandatory = states)
    loaded <- cohort2TrajectoryConfiguration(
      studyName = "settings", pathToStudy = path, useCDM = FALSE, runSavedStudy = TRUE
    )
    expect_identical(list(loaded$stateCohortAbsorbing, loaded$stateCohortMandatory),
                     list(states, states))
  }
  expect_identical(list.files(file.path(path, "settings", "Settings")), "analysisSettings.rds")
  # Legacy CSVs must neither affect a saved run nor be overwritten.
  legacyFile <- file.path(path, "settings", "Settings", "trajectorySettings.csv")
  writeLines("legacy settings left untouched", legacyFile)
  createTrajectories(studyEnv = study)
  expect_identical(readLines(legacyFile), "legacy settings left untouched")
  loaded <- cohort2TrajectoryConfiguration(
    studyName = "settings", pathToStudy = path, useCDM = FALSE, runSavedStudy = TRUE
  )
  expect_identical(loaded$allowedStatesList, study$allowedStatesList)
})
