test_that("discrete definitions retain unobserved states and effective generation choices", {
  root <- tempfile("c2t-definition-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", "A"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 20)
  )
  labels <- c("Absent", "A", "Death")
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, studyName = "discrete", useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = labels,
    stateCohortPriorityOrder = labels, stateSelectionType = "Priority",
    trajectoryType = "Discrete", cycleLength = 3,
    stateCohortAbsorbing = "Death", allowedStatesList = list()
  )
  getDataForStudy(studyEnv = study)
  result <- createTrajectories(
    studyEnv = study, cycleLength = 7, oocFix = "Fallback",
    stateCohortAbsorbing = c("Death", "Fallback")
  )
  definition <- list(
    version = 1L, trajectoryType = "Discrete", cycleLength = 7L,
    states = c("Absent", "A", "Death", "Fallback"),
    absorbingStates = c("Death", "Fallback")
  )
  expect_identical(attr(result, "trajectory_definition"), definition)
  expect_identical(result$state_label, c("START", "A", "Fallback", "EXIT"))
  expect_s3_class(result, "c2t_trajectories")

  # Data-frame row operations retain the complete definition, even if no rows
  # or only one clinical state remain. RDS preserves both class and attributes.
  for (rows in list(result$state_label == "A", FALSE, rev(seq_len(nrow(result))))) {
    subset <- result[rows, , drop = FALSE]
    expect_identical(attr(subset, "trajectory_definition"), definition)
    expect_s3_class(subset, "c2t_trajectories")
  }
  rds <- file.path(root, "trajectories.rds")
  saveRDS(result, rds)
  expect_identical(readRDS(rds), result)

  # The handoff is attached only to the returned object; CSV remains ordinary
  # tabular data and saved study settings retain their existing structure.
  ordinary <- as.data.frame(result)
  attr(ordinary, "trajectory_definition") <- NULL
  csv <- file.path(root, "discrete", "Data", "patientDataDiscrete.csv")
  expectedCsv <- file.path(root, "ordinary.csv")
  save_object(ordinary, expectedCsv)
  expect_identical(unname(tools::md5sum(csv)), unname(tools::md5sum(expectedCsv)))
  expect_null(attr(read.csv(csv), "trajectory_definition"))
  saved <- readRDS(file.path(root, "discrete", "Settings", "analysisSettings.rds"))
  expect_identical(names(saved), c("version", "import", "generation", "definitions",
                                   "definitionContents", "stateDictionary"))
  expect_identical(saved$version, 6L)
  expect_equal(saved$generation$cycleLength, 7)
  expect_identical(saved$generation$stateCohortAbsorbing, c("Death", "Fallback"))
  expect_false("trajectory_definition" %in% names(saved))
})

test_that("merged definitions use resolved labels and absorbing overrides", {
  root <- tempfile("c2t-merged-definition-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  labels <- c("A", "B", "C", "Absent")
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", "B", "C"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 0, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 6, 20)
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, studyName = "merged", useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = labels,
    stateCohortPriorityOrder = labels, stateSelectionType = "Priority",
    trajectoryType = "Discrete", cycleLength = 7,
    stateCohortAbsorbing = "A", mergeStates = TRUE,
    mergedStateOverrides = list(
      mergedState(c("A", "B"), absorbing = FALSE, label = "Combined"),
      mergedState(c("B", "C"), absorbing = TRUE, label = "Unobserved terminal")
    )
  )
  getDataForStudy(studyEnv = study)
  result <- createTrajectories(studyEnv = study, oocFix = "Literal gap")
  definition <- attr(result, "trajectory_definition")
  expect_identical(definition, list(
    version = 1L, trajectoryType = "Discrete", cycleLength = 7L,
    states = c("A", "B", "C", "Absent", "Combined", "Unobserved terminal", "Literal gap"),
    absorbingStates = c("A", "Unobserved terminal")
  ))
  expect_identical(result$state_label, c("START", "Combined", "OUT OF COHORT", "C", "EXIT"))
  expect_false(any(definition$states %in% study$stateDictionary$state_key))

  # Generation-time renaming and absorbing overrides replace the imported
  # display labels and inherited flags in the definition too.
  changed <- createTrajectories(
    studyEnv = study,
    mergedStateOverrides = list(mergedState(c("A", "B"), absorbing = TRUE,
                                           label = "Renamed terminal"))
  )
  changedDefinition <- attr(changed, "trajectory_definition")
  expect_true("Renamed terminal" %in% changedDefinition$states)
  expect_false("Combined" %in% changedDefinition$states)
  expect_identical(changedDefinition$absorbingStates, c("A", "Renamed terminal"))
  expect_identical(changed$state_label, c("START", "Renamed terminal", "EXIT"))
})

test_that("continuous definitions have no discrete cycle length", {
  root <- tempfile("c2t-continuous-definition-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A"),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-01") + c(13, 6)
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, studyName = "continuous", useCDM = FALSE,
    trajectoryDataObject = input, stateCohortLabels = c("A", "Absent"),
    stateCohortPriorityOrder = c("A", "Absent"), stateSelectionType = "Priority",
    trajectoryType = "Continuous", cycleLength = 7,
    stateCohortAbsorbing = "Absent"
  )
  getDataForStudy(studyEnv = study)
  result <- createTrajectories(studyEnv = study)
  expect_identical(attr(result, "trajectory_definition"), list(
    version = 1L, trajectoryType = "Continuous", cycleLength = NULL,
    states = c("A", "Absent"), absorbingStates = "Absent"
  ))
  withoutAbsorption <- createTrajectories(studyEnv = study, stateCohortAbsorbing = NULL)
  expect_identical(attr(withoutAbsorption, "trajectory_definition")$absorbingStates,
                   character(0))
})

test_that("episodic definitions declare episode identity without patient data", {
  base <- list(
    version = 1L, trajectoryType = "Discrete", cycleLength = 7L,
    states = c("A", "Death"), absorbingStates = "Death"
  )
  states <- c("START", "A", "Death", "OUT OF COHORT", "EXIT")
  expect_identical(.trajectoryDefinition("Discrete", 7, states, "Death"), base)
  expect_identical(.trajectoryDefinition(
    "Discrete", 7, states, "Death", targetEpisodeHandling = "First",
    stopAfterAbsorbingEpisode = TRUE
  ), base)
  for (stop in c(FALSE, TRUE)) {
    expected <- base
    expected$version <- 2L
    expected$identityColumns <- c("subject_id", "episode_id")
    expected$timeOrigin <- "episode_start"
    expected$targetEpisodeHandling <- "All"
    expected$stopAfterAbsorbingEpisode <- stop
    actual <- .trajectoryDefinition(
      "Discrete", 7, states, "Death", targetEpisodeHandling = "All",
      stopAfterAbsorbingEpisode = stop
    )
    expect_identical(actual, expected)
    continuous <- .trajectoryDefinition(
      "Continuous", 7, states, "Death", targetEpisodeHandling = "All",
      stopAfterAbsorbingEpisode = stop
    )
    expect_null(continuous$cycleLength)
    expect_identical(continuous$identityColumns, c("subject_id", "episode_id"))
    expect_identical(continuous$timeOrigin, "episode_start")
  }
})
