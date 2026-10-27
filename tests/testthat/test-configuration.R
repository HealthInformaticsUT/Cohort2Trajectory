test_that("valid input is accepted and configuration choices ignore case", {
  path <- tempfile("c2t-config-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  data <- data.frame(subject_id = 1L, cohort_definition_id = "A",
                     cohort_start_date = as.Date("2020-01-01"),
                     cohort_end_date = as.Date("2020-01-07"))
  expect_warning(study <- cohort2TrajectoryConfiguration(
    pathToStudy = path, useCDM = FALSE, trajectoryDataObject = data,
    trajectoryType = " discrete ", stateSelectionType = "priority"
  ), NA)
  expect_equal(c(study$trajectoryType, study$stateSelectionType), c(0, 3))
})

test_that("incorrect column types produce the appropriate warning", {
  path <- tempfile("c2t-config-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  data <- data.frame(subject_id = 1L, cohort_definition_id = "A",
                     cohort_start_date = as.Date("2020-01-01"),
                     cohort_end_date = as.Date("2020-01-07"))
  invalid <- list(subject_id = TRUE, cohort_definition_id = 1L,
                  cohort_start_date = "2020-01-01", cohort_end_date = "2020-01-07")
  for (column in names(invalid)) {
    input <- data
    input[[column]] <- invalid[[column]]
    expect_warning(cohort2TrajectoryConfiguration(
      studyName = column, pathToStudy = path, useCDM = FALSE,
      trajectoryDataObject = input
    ), paste0(column, " column"))
  }
})

test_that("unknown choices fail before creating study files", {
  path <- tempfile("c2t-config-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  expect_error(cohort2TrajectoryConfiguration(
    pathToStudy = path, trajectoryType = "typo"
  ), "trajectoryType must be one of: Discrete, Continuous")
  expect_error(cohort2TrajectoryConfiguration(
    pathToStudy = path, stateSelectionType = "typo"
  ), "stateSelectionType must be one of: First, Overlap, Priority")
  expect_length(list.files(path), 0)
})
