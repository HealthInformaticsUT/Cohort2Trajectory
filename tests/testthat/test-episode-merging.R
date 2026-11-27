test_that("merged absorbing overrides apply within episodes before optional patient cutoff", {
  root <- tempfile("c2t-episode-merging-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  origin <- as.Date("2020-01-01")
  input <- data.frame(
    subject_id = c(rep("001", 5), rep("002", 4)),
    cohort_definition_id = c("0", "0", "A", "B", "A", "0", "0", "A", "A"),
    cohort_start_date = origin + c(0, 14, 0, 1, 14, 0, 14, 0, 14),
    cohort_end_date = origin + c(6, 20, 6, 6, 20, 6, 20, 6, 20)
  )
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE,
      trajectoryDataObject = input, targetEpisodeHandling = "All",
      stateCohortLabels = c("A", "B"), stateCohortPriorityOrder = c("B", "A"),
      stateSelectionType = "Priority", trajectoryType = type, cycleLength = 7,
      mergeStates = TRUE, batchSize = 1,
      mergedStateOverrides = list(mergedState(c("A", "B"),
        label = "Combined", absorbing = TRUE, allowedTransitions = NULL))
    )
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_true("Combined" %in% result$state_label)
    expect_equal(result$episode_id[result$state_label == "START"], rep(1:2, 2))
    expect_equal(attr(result, "trajectory_definition")$absorbingStates, "Combined")
    source <- readRDS(file.path(root, type, "Data", "sourceIntervals.rds"))
    expect_equal(sum(source$cohort_definition_id == "0"), 4L)
    expect_equal(source$episode_id[source$cohort_definition_id == "0"], rep(1:2, 2))

    suppressed <- createTrajectories(studyEnv = study, stopAfterAbsorbingEpisode = TRUE)
    expect_equal(unique(suppressed$episode_id[suppressed$subject_id == "001"]), 1L)
    expect_equal(unique(suppressed$episode_id[suppressed$subject_id == "002"]), 1:2)
    restored <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE,
      trajectoryDataObject = input, runSavedStudy = TRUE
    )
    getDataForStudy(studyEnv = restored)
    expect_equal(createTrajectories(studyEnv = restored), suppressed)
  }
})

test_that("episode rows remain chronological when state occurrence counts repeat", {
  root <- tempfile("c2t-episode-order-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  origin <- as.Date("2020-01-01")
  input <- data.frame(
    subject_id = "001", cohort_definition_id = c("0", "0", "A", "A", "B", "A"),
    cohort_start_date = origin + c(0, 31, 0, 4, 7, 31),
    cohort_end_date = origin + c(13, 44, 2, 6, 13, 44)
  )
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE,
      trajectoryDataObject = input, targetEpisodeHandling = "All",
      stateCohortLabels = c("A", "B"), stateCohortPriorityOrder = c("B", "A"),
      stateSelectionType = "Priority", trajectoryType = type, cycleLength = 3
    )
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_true(any(result$seq_ordinal > 1))
    for (episode in split(as.data.frame(result), result$episode_id)) {
      expect_equal(episode$state_start_date, sort(episode$state_start_date))
      expect_equal(head(episode$state_label, 1), "START")
      expect_equal(tail(episode$state_label, 1), "EXIT")
    }
  }
})
