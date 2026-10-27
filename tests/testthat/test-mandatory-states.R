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
