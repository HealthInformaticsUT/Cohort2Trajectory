test_that("invalid gap fallbacks fail before creating or overwriting study files", {
  path <- tempfile("c2t-gap-validation-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  invalid <- list("START", "exit", "0", " ", NA_character_, NULL, c("A", "B"), 1)
  for (value in invalid) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = "invalid", useCDM = FALSE, oocFix = value
    ), "oocFix")
  }
  expect_false(dir.exists(file.path(path, "invalid")))
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 7),
    cohort_end_date = as.Date("2020-01-01") + c(20, 13)
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "valid", useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = "A",
    stateCohortPriorityOrder = "A", stateSelectionType = "Priority", lengthOfStay = 7
  )
  getDataForStudy(studyEnv = study)
  gaps <- createTrajectories(studyEnv = study)
  expect_equal(createTrajectories(studyEnv = study, oocFix = "OUT OF COHORT"), gaps)
  files <- file.path(path, "valid", c("Data/patientDataDiscrete.csv", "Settings/analysisSettings.rds"))
  before <- tools::md5sum(files)
  for (value in invalid) {
    expect_error(createTrajectories(studyEnv = study, oocFix = value), "oocFix")
  }
  expect_identical(tools::md5sum(files), before)
  expect_error(getTrajectoriesDiscrete(NULL, input, stateDuration = 7,
    statePriorityVector = "A", oocFix = "EXIT", addPersonalData = FALSE), "oocFix")
  snapshot <- readRDS(files[2])
  snapshot$generation$oocFix <- "EXIT"
  saveRDS(snapshot, files[2])
  expect_error(cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "valid", useCDM = FALSE, runSavedStudy = TRUE
  ), "oocFix")
})
