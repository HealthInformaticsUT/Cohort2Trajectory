test_that("ambiguous transition source names fail before creating study files", {
  path <- tempfile("c2t-invalid-sources-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  invalid <- list(
    list(A = "B", A = "C"), list(A = "C", A = "B"),
    list("B"), list(A = "B", "C"),
    setNames(list("B"), NA_character_), setNames(list("B"), " ")
  )
  for (rules in invalid) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, useCDM = FALSE, allowedStatesList = rules
    ), "allowedStatesList.*(source entries|source state)")
  }
  expect_length(list.files(path, all.files = TRUE, no.. = TRUE), 0)
})

test_that("duplicate source overrides and saved rules cannot silently change trajectories", {
  path <- tempfile("c2t-source-rules-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", "B", "C"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 13, 20)
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "rules", useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = c("A", "B", "C"),
    stateCohortPriorityOrder = c("A", "B", "C"), stateSelectionType = "Priority",
    trajectoryType = "Continuous", stateCohortAbsorbing = "B",
    allowedStatesList = list(A = "b")
  )
  getDataForStudy(studyEnv = study)
  expect_identical(createTrajectories(studyEnv = study, allowedStatesList = list(A = NULL))$state_label,
                    c("START", "A", "EXIT"))
  # A partial list is valid: no outgoing entry is needed after absorbing B.
  expect_identical(createTrajectories(studyEnv = study)$state_label, c("START", "A", "B", "EXIT"))
  files <- file.path(path, "rules", c("Data/importedDataCleaned_.csv",
                                      "Data/patientDataContinuous.csv", "Settings/analysisSettings.rds"))
  before <- tools::md5sum(files)
  duplicate <- list(A = "B", A = "C", B = "C", C = "C")
  expect_error(createTrajectories(studyEnv = study, allowedStatesList = duplicate),
                "duplicate source entries.*A")
  expect_error(getDataForStudy(studyEnv = study, allowedStatesList = duplicate),
                "duplicate source entries.*A")
  expect_identical(tools::md5sum(files), before)

  baseline <- readRDS(files[3])
  for (phase in c("import", "generation")) {
    snapshot <- baseline
    snapshot[[phase]]$allowedStatesList <- duplicate
    saveRDS(snapshot, files[3])
    checksum <- tools::md5sum(files[3])
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = "rules", useCDM = FALSE, runSavedStudy = TRUE
    ), paste0("Saved ", phase, " allowedStatesList has duplicate source entries"))
    expect_identical(tools::md5sum(files[3]), checksum)
  }
})
