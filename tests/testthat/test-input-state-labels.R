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
