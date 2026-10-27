test_that("optional state lists survive saving, updating and reloading settings", {
  path <- tempfile("c2t-settings-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  study <- cohort2TrajectoryConfiguration(
    studyName = "settings", pathToStudy = path, useCDM = FALSE,
    atlasTargetCohort = "0", stateCohortLabels = c("A", "B"),
    stateCohortPriorityOrder = c("A", "B"), stateSelectionType = "Priority", lengthOfStay = 7
  )
  data <- data.frame(subject_id = c(1L, 1L), cohort_definition_id = c("0", "A"),
                     cohort_start_date = as.Date("2020-01-01"),
                     cohort_end_date = as.Date("2020-01-07"), time_in_cohort = 0)
  utils::write.csv(data, file.path(path, "settings", "Data", "importedDataCleaned_0.csv"),
                   row.names = FALSE)
  for (states in list(NULL, c("A", "B"), character(0))) {
    createTrajectories(studyEnv = study, stateCohortAbsorbing = states,
                       stateCohortMandatory = states)
    loaded <- loadSettings("settings", path)
    expected <- if (length(states)) states else NULL
    expect_identical(list(loaded$savedAbsorbingStates, loaded$savedMandatoryStates),
                     list(expected, expected))
  }
  settingsFile <- file.path(path, "settings", "Settings", "trajectorySettings.csv")
  settings <- utils::read.csv(settingsFile)
  expect_equal(dim(settings), c(1L, 10L))
  # Older/manual files can contain missing cells or the literal string NULL.
  settings$absorbingStates <- NA_character_
  settings$mandatoryStates <- "NULL"
  utils::write.csv(settings, settingsFile, row.names = FALSE)
  loaded <- loadSettings("settings", path)
  expect_identical(list(loaded$savedAbsorbingStates, loaded$savedMandatoryStates),
                   list(NULL, NULL))
})
