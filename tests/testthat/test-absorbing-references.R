test_that("absorbing references are checked before either public format is saved", {
  root <- tempfile("c2t-absorbing-references-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", "Death", "Later"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 13, 20)
  )
  labels <- c("A", "Death", "Later", "Absent")
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = root, studyName = type, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = labels,
      stateCohortPriorityOrder = labels, stateSelectionType = "Priority",
      trajectoryType = type, lengthOfStay = 7, stateCohortAbsorbing = "Death",
      allowedStatesList = list()
    )
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_equal(tail(result$state_start_date, 1), as.Date("2020-01-15"))
    files <- file.path(root, type, c(paste0("Data/patientData", type, ".csv"),
                                    "Settings/analysisSettings.rds"))
    before <- tools::md5sum(files)
    for (invalid in c("Deth", "death", "0")) {
      expect_error(createTrajectories(studyEnv = study, stateCohortAbsorbing = invalid),
                    "stateCohortAbsorbing contains unknown state labels")
    }
    expect_identical(tools::md5sum(files), before)

    absent <- createTrajectories(studyEnv = study, stateCohortAbsorbing = "Absent")
    expect_equal(tail(absent$state_start_date, 1), as.Date("2020-01-22"))
    expect_equal(createTrajectories(studyEnv = study, stateCohortAbsorbing = NULL),
                  createTrajectories(studyEnv = study, stateCohortAbsorbing = character(0)))
    observed <- createTrajectories(studyEnv = study, stateCohortLabels = "Absent")
    expect_equal(tail(observed$state_start_date, 1), as.Date("2020-01-15"))

    if (type == "Discrete") {
      getDataForStudy(studyEnv = study, trajectoryDataObject = input[c(1, 2, 4), ])
      fallback <- createTrajectories(studyEnv = study, oocFix = "Fallback",
                                     stateCohortAbsorbing = "Fallback")
      expect_identical(fallback$state_label, c("START", "A", "Fallback", "EXIT"))
      expect_equal(tail(fallback$state_start_date, 1), as.Date("2020-01-15"))
    }
  }
})
