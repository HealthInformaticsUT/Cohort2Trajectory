test_that("data-frame and CSV imports preserve identical clinical state labels", {
  path <- tempfile("c2t-state-labels-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  labels <- c("Recorded pneumonia", "Recorded_pneumonia", "Feeding / airway")
  input <- data.frame(
    subject_id = rep("001", 4), cohort_definition_id = c("0", labels),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 13, 20)
  )
  inputFile <- file.path(path, "input.csv")
  write.csv(input, inputFile, row.names = FALSE)
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = type, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = labels,
      stateCohortPriorityOrder = labels, stateCohortMandatory = labels[1],
      stateSelectionType = "Priority", trajectoryType = type, lengthOfStay = 7
    )
    expect_warning(getDataForStudy(studyEnv = study), NA)
    fromObject <- createTrajectories(studyEnv = study)
    expect_identical(fromObject$state_label, c("START", labels, "EXIT"))

    getDataForStudy(studyEnv = study, trajectoryDataObject = NULL, pathToData = inputFile)
    expect_equal(createTrajectories(studyEnv = study), fromObject)
  }
})

test_that("ambiguous configured labels fail before creating study files", {
  path <- tempfile("c2t-invalid-labels-")
  for (labels in list("START", "exit", "OUT OF COHORT", "0",
                      c("A", "A"), c("A", "a"), " ", NA_character_)) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, useCDM = FALSE, stateCohortLabels = labels
    ), "stateCohortLabels")
  }
  expect_false(dir.exists(path))
  expect_error(mergedState(c("A", "B"), label = "START"), "reserved")
})

test_that("clinical boundary labels cannot be silently dropped by either engine", {
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "START"),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-08")),
    cohort_end_date = as.Date(c("2020-01-21", "2020-01-14"))
  )
  expect_error(Cohort2Trajectory:::cleanCohortData(input, NULL), "reserved")
  expect_error(Cohort2Trajectory:::getTrajectoriesContinuous(
    NULL, input, stateSelection = 3, statePriorityVector = "START",
    addPersonalData = FALSE
  ), "reserved")
  expect_error(Cohort2Trajectory:::getTrajectoriesDiscrete(
    NULL, input, stateDuration = 7, stateSelection = 3,
    statePriorityVector = "START", addPersonalData = FALSE
  ), "reserved")
  input$cohort_definition_id[2] <- NA_character_
  expect_error(Cohort2Trajectory:::cleanCohortData(input, NULL), "non-missing")
})

test_that("import and generation reject case collisions across patients", {
  path <- tempfile("c2t-label-collision-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = c(1L, 1L, 2L, 2L), cohort_definition_id = c("0", "A", "0", "A"),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-07")
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "labels", useCDM = FALSE, atlasTargetCohort = "0",
    trajectoryDataObject = input, stateCohortLabels = "A",
    stateCohortPriorityOrder = "A", stateSelectionType = "Priority",
    trajectoryType = "Discrete", lengthOfStay = 7, batchSize = 1
  )
  getDataForStudy(studyEnv = study)
  expect_silent(Cohort2Trajectory:::.validateObservedStateLabels(c("0", "A", "A")))
  createTrajectories(studyEnv = study)
  savedPath <- file.path(path, "labels", "Settings", "analysisSettings.rds")
  saved <- readRDS(savedPath)
  saved$import$stateCohortLabels <- "START"
  saveRDS(saved, savedPath)
  expect_error(cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "labels", runSavedStudy = TRUE, useCDM = FALSE
  ), "reserved")

  cleanedPath <- file.path(path, "labels", "Data", "importedDataCleaned_0.csv")
  cleaned <- read.csv(cleanedPath)
  cleaned$cohort_definition_id[cleaned$subject_id == 2 & cleaned$cohort_definition_id == "A"] <- "a"
  write.csv(cleaned, cleanedPath, row.names = FALSE)
  expect_error(createTrajectories(studyEnv = study), "ignoring case")
  input$cohort_definition_id[4] <- "a"
  expect_error(getDataForStudy(studyEnv = study, trajectoryDataObject = input), "ignoring case")
})
