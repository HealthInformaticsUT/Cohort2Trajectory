test_that("failed imports cannot reuse old patients and a successful retry recovers", {
  path <- tempfile("c2t-import-retry-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(subject_id = "old_patient", cohort_definition_id = "0",
                       cohort_start_date = as.Date("2020-01-01"),
                       cohort_end_date = as.Date("2020-01-14"))
  invalid <- input
  invalid$cohort_end_date <- as.Date("2019-12-31")
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = type, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = "A",
      stateCohortPriorityOrder = "A", stateSelectionType = "Priority",
      trajectoryType = type, lengthOfStay = 7
    )
    getDataForStudy(studyEnv = study)
    createTrajectories(studyEnv = study)
    files <- file.path(path, type, c("Data/importedDataCleaned_.csv",
                                    paste0("Data/patientData", type, ".csv"),
                                    "Settings/analysisSettings.rds"))
    before <- tools::md5sum(files)

    expect_warning(getDataForStudy(studyEnv = study, trajectoryDataObject = input[FALSE, ]),
                    "There were no patients imported")
    expect_error(createTrajectories(studyEnv = study), "latest import did not complete successfully")
    expect_error(getDataForStudy(studyEnv = study, trajectoryDataObject = invalid),
                  "end >= start")
    expect_error(createTrajectories(studyEnv = study), "latest import did not complete successfully")
    expect_identical(tools::md5sum(files), before)

    replacement <- input
    replacement$subject_id <- "new_patient"
    getDataForStudy(studyEnv = study, trajectoryDataObject = replacement)
    expect_identical(unique(createTrajectories(studyEnv = study)$subject_id), "new_patient")
  }
})

test_that("an empty eligible cohort does not replace successful import artifacts", {
  path <- tempfile("c2t-empty-import-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = "old_patient", cohort_definition_id = c("0", "A", "B"),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-14")
  )
  for (merged in c(FALSE, TRUE)) {
    name <- if (merged) "merged" else "ordinary"
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = name, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = c("A", "B", "C"),
      stateCohortPriorityOrder = c("A", "B", "C"), stateSelectionType = "Priority",
      lengthOfStay = 7, mergeStates = merged
    )
    getDataForStudy(studyEnv = study)
    createTrajectories(studyEnv = study)
    files <- file.path(path, name, c(
      "Data/importedDataCleaned_.csv", "Data/patientDataDiscrete.csv",
      "Settings/analysisSettings.rds",
      if (merged) c("Data/sourceIntervals.rds", "Data/stateDictionary.rds")
    ))
    before <- tools::md5sum(files)
    dictionary <- study$stateDictionary

    expect_warning(getDataForStudy(studyEnv = study, stateCohortMandatory = "C"),
                    "No patients left after cleaning")
    expect_false(study$.dataReady)
    expect_error(createTrajectories(studyEnv = study), "latest import did not complete successfully")
    expect_identical(tools::md5sum(files), before)
    expect_identical(study$stateDictionary, dictionary)

    replacement <- input
    replacement$subject_id <- "new_patient"
    getDataForStudy(studyEnv = study, trajectoryDataObject = replacement)
    expect_identical(unique(createTrajectories(studyEnv = study)$subject_id), "new_patient")
  }
})
