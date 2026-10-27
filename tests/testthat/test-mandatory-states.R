test_that("mandatory labels are literal and all requirements are applied", {
  path <- tempfile("c2t-mandatory-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
    cohort_definition_id = c("0", "A", "No mandatory state", "0", "A", "0", "No mandatory state"),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-07")
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "mandatory", useCDM = FALSE,
    trajectoryDataObject = input, atlasTargetCohort = "0",
    stateCohortLabels = c("A", "No mandatory state")
  )
  retained <- function(required) {
    getDataForStudy(studyEnv = study, stateCohortMandatory = required)
    cleaned <- .readPatientCsv(file.path(path, "mandatory", "Data", "importedDataCleaned_0.csv"))
    sort(unique(cleaned$subject_id))
  }
  expect_equal(retained(c("A", "No mandatory state")), 1)
  expect_equal(retained("No mandatory state"), c(1, 3))
  expect_equal(retained(NULL), c(1, 2, 3))
  expect_equal(retained(character(0)), c(1, 2, 3))
})

test_that("generation cannot silently change mandatory-state eligibility", {
  path <- tempfile("c2t-mandatory-generation-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = c(1L, 1L, 2L, 2L), cohort_definition_id = c("0", "A", "0", "B"),
    cohort_start_date = as.Date("2020-01-01"), cohort_end_date = as.Date("2020-01-07")
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = path, studyName = "mandatory", useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = c("A", "B"),
    stateCohortPriorityOrder = c("A", "B"), stateSelectionType = "Priority", lengthOfStay = 7
  )
  expect_error(createTrajectories(studyEnv = study, stateCohortMandatory = "A"),
                "requires a successful getDataForStudy")
  getDataForStudy(studyEnv = study)
  expect_equal(unique(createTrajectories(studyEnv = study, stateCohortMandatory = character(0))$subject_id), c(1, 2))
  files <- file.path(path, "mandatory", c("Data/patientDataDiscrete.csv", "Settings/analysisSettings.rds"))
  before <- tools::md5sum(files)
  expect_error(createTrajectories(studyEnv = study, stateCohortMandatory = "A"),
                "differs from.*import")
  expect_identical(tools::md5sum(files), before)

  getDataForStudy(studyEnv = study, stateCohortMandatory = "A")
  expect_equal(unique(createTrajectories(studyEnv = study, stateCohortMandatory = c("A", "A"))$subject_id), 1)
  before <- tools::md5sum(files)
  expect_error(createTrajectories(studyEnv = study, stateCohortMandatory = NULL),
                "differs from.*import")
  expect_identical(tools::md5sum(files), before)
  # Editing the environment must not bypass the recorded import settings.
  study$stateCohortMandatory <- NULL
  expect_error(createTrajectories(studyEnv = study), "differs from.*import")
  getDataForStudy(studyEnv = study)
  expect_equal(unique(createTrajectories(studyEnv = study)$subject_id), c(1, 2))

  snapshot <- readRDS(files[2])
  for (version in c(1L, 3L)) {
    snapshot$version <- version
    snapshot$generation$stateCohortMandatory <- "A"
    saveRDS(snapshot, files[2])
    before <- tools::md5sum(files[2])
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = "mandatory", useCDM = FALSE, runSavedStudy = TRUE
    ), "requirements disagree between import and generation")
    expect_identical(tools::md5sum(files[2]), before)
  }
})
