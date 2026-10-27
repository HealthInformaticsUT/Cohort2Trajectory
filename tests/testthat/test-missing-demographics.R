test_that("non-CDM trajectories leave demographics missing in output and CSV", {
  path <- tempfile("c2t-demographics-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = "patient-1", cohort_definition_id = c("0", "A"),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-07")
  )
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = type, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = "A",
      stateCohortPriorityOrder = "A", stateSelectionType = "Priority",
      trajectoryType = type, lengthOfStay = 7
    )
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_identical(result$age, rep(NA_real_, nrow(result)))
    expect_identical(result$gender_concept_id, rep(NA_integer_, nrow(result)))
    expect_false("birth_datetime" %in% names(result))

    saved <- read.csv(file.path(path, type, "Data", paste0("patientData", type, ".csv")))
    expect_identical(names(saved), names(result))
    expect_true(all(is.na(saved[c("age", "gender_concept_id")])))
  }
})
