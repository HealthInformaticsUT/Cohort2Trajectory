test_that("a later batch failure does not publish partial trajectories", {
  path <- tempfile("c2t-batch-output-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = c(1L, 1L, 2L, 2L), cohort_definition_id = c("0", "A", "0", "B"),
    cohort_start_date = as.Date("2020-01-01"), cohort_end_date = as.Date("2020-01-07")
  )
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = type, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = c("A", "B"),
      stateCohortPriorityOrder = c("A", "B"), stateSelectionType = "Priority",
      trajectoryType = type, lengthOfStay = 7, batchSize = 1
    )
    getDataForStudy(studyEnv = study)
    expected <- createTrajectories(studyEnv = study)
    output <- file.path(path, type, "Data", paste0("patientData", type, ".csv"))
    files <- c(output, file.path(path, type, "Settings", "analysisSettings.rds"))
    before <- tools::md5sum(files)
    # Batch 1 is valid. Only patient 2 has the unranked B state.
    expect_error(createTrajectories(studyEnv = study, stateCohortPriorityOrder = "A"),
                  "statePriorityVector")
    expect_identical(tools::md5sum(files), before)

    unlink(output)
    expect_error(createTrajectories(studyEnv = study, stateCohortPriorityOrder = "A"),
                  "statePriorityVector")
    expect_false(file.exists(output))
    expect_equal(createTrajectories(studyEnv = study), expected)
    expect_equal(read.csv(output)$subject_id, expected$subject_id)
  }
})
