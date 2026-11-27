test_that("saved episodic studies restore import and generation policies", {
  root <- tempfile("c2t-episode-settings-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  input <- data.frame(
    subject_id = "001", cohort_definition_id = c("0", "A", "0", "A"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 14, 14),
    cohort_end_date = as.Date("2020-01-01") + c(6, 6, 20, 20)
  )
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = "A",
      stateCohortPriorityOrder = "A", stateSelectionType = "Priority",
      trajectoryType = type, cycleLength = 7,
      targetEpisodeHandling = " all ", stopAfterAbsorbingEpisode = TRUE
    )
    getDataForStudy(studyEnv = study)
    expected <- createTrajectories(studyEnv = study)
    path <- file.path(root, type, "Settings", "analysisSettings.rds")
    checksum <- tools::md5sum(path)
    saved <- readRDS(path)
    expect_identical(saved$version, 6L)
    expect_identical(saved$import$targetEpisodeHandling, "All")
    expect_true(saved$generation$stopAfterAbsorbingEpisode)
    expect_false("targetEpisodeHandling" %in% names(saved$generation))
    expect_false("stopAfterAbsorbingEpisode" %in% names(saved$import))

    restored <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE,
      trajectoryDataObject = input, runSavedStudy = TRUE
    )
    expect_identical(restored$targetEpisodeHandling, "All")
    expect_true(restored$stopAfterAbsorbingEpisode)
    expect_error(getDataForStudy(studyEnv = restored, targetEpisodeHandling = "First"),
                 "Do not also supply: targetEpisodeHandling")
    getDataForStudy(studyEnv = restored)
    expect_error(createTrajectories(studyEnv = restored, stopAfterAbsorbingEpisode = FALSE),
                 "Do not also supply: stopAfterAbsorbingEpisode")
    expect_equal(createTrajectories(studyEnv = restored), expected)
    expect_identical(tools::md5sum(path), checksum)
    expect_error(cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE,
      runSavedStudy = TRUE, targetEpisodeHandling = "All"
    ), "Do not also supply: targetEpisodeHandling")
  }
})

test_that("version 5 studies gain first-episode defaults without rewriting", {
  root <- tempfile("c2t-v5-episodes-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  study <- cohort2TrajectoryConfiguration(
    studyName = "legacy", pathToStudy = root, useCDM = FALSE,
    stateCohortLabels = "A", stateCohortPriorityOrder = "A",
    stateSelectionType = "Priority", cycleLength = 7
  )
  imported <- mget(.studyImportFields(), envir = study)
  imported$targetEpisodeHandling <- NULL
  generation <- mget(.studyGenerationFields(), envir = study)
  generation$stopAfterAbsorbingEpisode <- NULL
  old <- list(version = 5L, import = imported, generation = generation,
              definitions = NULL, definitionContents = NULL, stateDictionary = NULL)
  path <- file.path(root, "legacy", "Settings", "analysisSettings.rds")
  saveRDS(old, path)
  checksum <- tools::md5sum(path)
  restored <- .readStudySnapshot(root, "legacy")
  expected <- old
  expected$version <- 6L
  expected$import$targetEpisodeHandling <- "First"
  expected$generation$stopAfterAbsorbingEpisode <- FALSE
  expect_identical(restored, expected)
  expect_identical(tools::md5sum(path), checksum)
  expect_identical(readRDS(path), old)
})

test_that("version 6 validates saved episode policies and bounds", {
  root <- tempfile("c2t-episode-validation-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  study <- cohort2TrajectoryConfiguration(
    studyName = "episodes", pathToStudy = root, useCDM = FALSE,
    stateCohortLabels = "A", stateCohortPriorityOrder = "A",
    stateSelectionType = "Priority", cycleLength = 7,
    targetEpisodeHandling = "All"
  )
  .saveStudySnapshot(study, mget(.studyGenerationFields(), envir = study))
  path <- file.path(root, "episodes", "Settings", "analysisSettings.rds")
  valid <- readRDS(path)
  for (value in list(NULL, "Latest", "", NA_character_, c("First", "All"), FALSE)) {
    invalid <- valid
    invalid$import["targetEpisodeHandling"] <- list(value)
    saveRDS(invalid, path)
    expect_error(.readStudySnapshot(root, "episodes"), "targetEpisodeHandling")
  }
  for (value in list(NULL, NA, 1, c(TRUE, FALSE), "TRUE")) {
    invalid <- valid
    invalid$generation["stopAfterAbsorbingEpisode"] <- list(value)
    saveRDS(invalid, path)
    expect_error(.readStudySnapshot(root, "episodes"), "stopAfterAbsorbingEpisode")
  }
  for (section in c("import", "generation")) {
    invalid <- valid
    field <- if (section == "import") "targetEpisodeHandling" else "stopAfterAbsorbingEpisode"
    invalid[[section]][[field]] <- NULL
    saveRDS(invalid, path)
    expect_error(.readStudySnapshot(root, "episodes"), "Unsupported or incomplete saved-study")

    invalid <- valid
    invalid[[section]]$outOfCohortAllowed <- TRUE
    saveRDS(invalid, path)
    expect_error(.readStudySnapshot(root, "episodes"), "outOfCohortAllowed")
  }
  valid$import$targetEpisodeHandling <- " all "
  saveRDS(valid, path)
  checksum <- tools::md5sum(path)
  expect_identical(.readStudySnapshot(root, "episodes")$import$targetEpisodeHandling, "All")
  expect_identical(tools::md5sum(path), checksum)
})
