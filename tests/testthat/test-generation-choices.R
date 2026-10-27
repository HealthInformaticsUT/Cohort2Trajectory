test_that("generation accepts named choices and rejects invalid overrides without writes", {
  path <- tempfile("c2t-generation-choices-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(subject_id = 1L, cohort_definition_id = c("0", "A"),
                       cohort_start_date = as.Date("2020-01-01"),
                       cohort_end_date = as.Date("2020-01-07"))
  study <- cohort2TrajectoryConfiguration(
    studyName = "choices", pathToStudy = path, useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = "A", stateCohortPriorityOrder = "A",
    stateSelectionType = "Priority", lengthOfStay = 7
  )
  getDataForStudy(studyEnv = study)
  for (case in list(list(" discrete ", "first", 0, 1),
                    list("Discrete", "Overlap", 0, 2),
                    list("CONTINUOUS", "priority", 1, 3))) {
    expect_warning(byName <- createTrajectories(
      studyEnv = study, trajectoryType = case[[1]], stateSelectionType = case[[2]]
    ), NA)
    expect_warning(byCode <- createTrajectories(
      studyEnv = study, trajectoryType = case[[3]], stateSelectionType = case[[2]]
    ), "trajectoryType.*deprecated")
    expect_equal(byCode, byName)
    expect_warning(byCode <- createTrajectories(
      studyEnv = study, trajectoryType = case[[1]], stateSelectionType = case[[4]]
    ), "stateSelectionType.*deprecated")
    expect_equal(byCode, byName)
  }
  settingsFile <- file.path(path, "choices", "Settings", "analysisSettings.rds")
  snapshot <- readRDS(settingsFile)
  saved <- snapshot$generation
  expect_identical(snapshot$version, 3L)
  expect_identical(c(saved$trajectoryType, saved$stateSelectionType), c("Continuous", "Priority"))
  files <- c(settingsFile, file.path(path, "choices", "Data",
                                      c("patientDataDiscrete.csv", "patientDataContinuous.csv")))
  before <- tools::md5sum(files)
  invalid <- list(trajectoryType = list(2, "typo", NULL, NA_real_, c(0, 1)),
                   stateSelectionType = list(0, "typo", NULL, NA_real_, c(1, 3)))
  for (argument in names(invalid)) {
    for (value in invalid[[argument]]) {
      args <- c(list(studyEnv = study), stats::setNames(list(value), argument))
      expect_error(do.call(createTrajectories, args), paste0(argument, " must be one of:"))
    }
  }
  expect_identical(tools::md5sum(files), before)

  # Both current and legacy snapshots restore readable settings without warnings.
  for (version in c(3L, 1L)) {
    if (version == 1L) {
      snapshot$version <- 1L
      snapshot$generation$trajectoryType <- 1
      snapshot$generation$stateSelectionType <- 3
      saveRDS(snapshot, settingsFile)
    }
    checksum <- tools::md5sum(settingsFile)
    expect_warning(restored <- cohort2TrajectoryConfiguration(
      studyName = "choices", pathToStudy = path, useCDM = FALSE,
      trajectoryDataObject = input, runSavedStudy = TRUE
    ), NA)
    expect_identical(c(restored$trajectoryType, restored$stateSelectionType),
                     c("Continuous", "Priority"))
    getDataForStudy(studyEnv = restored)
    expect_warning(result <- createTrajectories(studyEnv = restored), NA)
    expect_equal(result, byName)
    expect_identical(tools::md5sum(settingsFile), checksum)
  }
})
