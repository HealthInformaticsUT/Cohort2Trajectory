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
  expect_equal(c(study$trajectoryType, study$stateSelectionType), c("Discrete", "Priority"))
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

test_that("unknown and numeric choices fail before creating study files", {
  path <- tempfile("c2t-config-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  for (value in list("typo", 0, 1, "0")) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, trajectoryType = value
    ), "trajectoryType must be one of: Discrete, Continuous")
  }
  for (value in list("typo", 1, 2, 3, "1")) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, stateSelectionType = value
    ), "stateSelectionType must be one of: First, Overlap, Priority")
  }
  expect_length(list.files(path), 0)
})


test_that("named state cohorts resolve to the existing configuration", {
  root <- tempfile("c2t-named-cohorts-")
  on.exit(unlink(root, recursive = TRUE))
  labels <- c("Stable", "Complication", "Dead")
  ids <- c(101, 102, 103)
  configure <- function(name, cohorts, stateLabels = NULL) {
    cohort2TrajectoryConfiguration(
      pathToStudy = root, studyName = name,
      atlasTargetCohort = 100, atlasStateCohorts = cohorts,
      stateCohortLabels = stateLabels,
      stateCohortPriorityOrder = rev(labels), stateCohortAbsorbing = "Dead",
      stateSelectionType = "Priority", cycleLength = 7
    )
  }
  named <- configure("named", setNames(ids, labels))
  explicit <- configure("explicit", ids, labels)
  repeated <- configure("repeated", setNames(ids, labels), labels)
  fields <- c("atlasStateCohorts", "stateCohortLabels", "allowedStatesList",
              "stateCohortPriorityOrder", "stateCohortAbsorbing")
  expect_identical(mget(fields, named), mget(fields, explicit))
  expect_identical(mget(fields, repeated), mget(fields, explicit))
  expect_identical(names(named$allowedStatesList), labels)
  expect_identical(named$stateCohortAbsorbing, "Dead")
})

test_that("ambiguous named mappings fail before creating study files", {
  path <- tempfile("c2t-invalid-mapping-")
  for (labels in list(c("A", ""), c("A", NA_character_), c("A", "a"),
                      c("A", "START"))) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, atlasStateCohorts = setNames(c(101, 102), labels)
    ), "Names of atlasStateCohorts")
  }
  expect_error(cohort2TrajectoryConfiguration(
    pathToStudy = path, atlasStateCohorts = c(A = 101, B = 102),
    stateCohortLabels = c("B", "A")
  ), "stateCohortLabels must match")
  expect_false(dir.exists(path))
})
