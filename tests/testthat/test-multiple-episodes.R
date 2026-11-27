episode_rows <- function(id, labels, starts, ends) {
  data.frame(
    subject_id = id,
    cohort_definition_id = labels,
    cohort_start_date = as.Date(starts),
    cohort_end_date = as.Date(ends)
  )
}

episode_study <- function(path, name, input, type, ...) {
  arguments <- utils::modifyList(list(
    pathToStudy = path, studyName = name, useCDM = FALSE,
    trajectoryDataObject = input, targetEpisodeHandling = "All",
    stateCohortLabels = c("A", "B", "Dead"),
    stateCohortPriorityOrder = c("Dead", "B", "A"),
    allowedStatesList = list(), stateSelectionType = "Priority",
    trajectoryType = type, cycleLength = 7
  ), list(...), keep.null = TRUE)
  do.call(cohort2TrajectoryConfiguration, arguments)
}

test_that("all target periods bound states independently without adding gap rows", {
  path <- tempfile("c2t-episodes-bounds-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- rbind(
    episode_rows("001", c("0", "0", "0", "A"),
                 c("2020-01-15", "2020-01-01", "2020-01-01", "2019-12-29"),
                 c("2020-01-21", "2020-01-07", "2020-01-07", "2020-01-24")),
    episode_rows("9007199254740993", c("0", "0"),
                 c("2020-01-02", "2020-01-16"), c("2020-01-08", "2020-01-22"))
  )
  for (type in c("Discrete", "Continuous")) {
    study <- episode_study(path, type, input, type)
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_setequal(unique(result$subject_id), c("001", "9007199254740993"))
    expect_type(result$episode_id, "integer")
    expect_equal(result$episode_id[result$state_label == "START"], rep(1:2, 2))
    patient <- result[result$subject_id == "001", ]
    expect_equal(patient$state_start_date[patient$state_label == "START"],
                 as.Date(c("2020-01-01", "2020-01-15")))
    expect_equal(patient$state_start_date[patient$state_label == "EXIT"],
                 as.Date(c("2020-01-08", "2020-01-22")))
    selected <- patient[patient$state_label == "A", ]
    expect_equal(selected$state_start_date, as.Date(c("2020-01-01", "2020-01-15")))
    expect_equal(selected$state_end_date, as.Date(c("2020-01-07", "2020-01-21")))
    expect_equal(selected$time_in_cohort, c(0, 0))
    expect_equal(selected$seq_ordinal, c(1, 1))
    expect_length(unique(selected$state_id), 1)
    targetOnly <- result[result$subject_id == "9007199254740993", ]
    expect_equal(targetOnly$state_label, if (type == "Discrete") {
      rep(c("START", "OUT OF COHORT", "EXIT"), 2)
    } else {
      rep(c("START", "EXIT"), 2)
    })
    expect_identical(attr(result, "trajectory_definition")$version, 2L)
  }
})

test_that("transitions and carry-forward restart at each episode", {
  path <- tempfile("c2t-episodes-reset-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- episode_rows("001", c("0", "0", "A", "B"),
                        c("2020-01-01", "2020-01-22", "2020-01-01", "2020-01-29"),
                        c("2020-01-14", "2020-02-04", "2020-01-07", "2020-02-04"))
  for (type in c("Discrete", "Continuous")) {
    study <- episode_study(
      path, type, input, type, allowedStatesList = list(A = "A", B = "B"),
      oocFix = "Last present state"
    )
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    clinical <- result[!result$state_label %in% c("START", "EXIT"), ]
    expect_equal(clinical$state_label, if (type == "Discrete") {
      c("A", "A", "OUT OF COHORT", "B")
    } else {
      c("A", "B")
    })
    expect_equal(clinical$seq_ordinal[clinical$state_label == "B"], 1)
    expect_equal(clinical$time_in_cohort[clinical$state_label == "B"], 7 / 365.25)
    expect_equal(result$time_in_cohort[result$state_label == "START"], c(0, 0))
  }
})

test_that("mandatory states filter episodes after chronological numbering", {
  path <- tempfile("c2t-episodes-mandatory-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- episode_rows(
    "001", c("0", "0", "0", "A", "B", "A", "B"),
    c("2020-02-01", "2020-01-01", "2020-01-15", "2020-01-01", "2020-01-15",
      "2020-02-01", "2020-02-02"),
    c("2020-02-07", "2020-01-07", "2020-01-21", "2020-01-07", "2020-01-21",
      "2020-02-07", "2020-02-07")
  )
  for (type in c("Discrete", "Continuous")) {
    study <- episode_study(path, type, input, type, stateCohortMandatory = "B")
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_equal(result$episode_id[result$state_label == "START"], 2:3)
    getDataForStudy(studyEnv = study, stateCohortMandatory = c("A", "B"))
    result <- createTrajectories(studyEnv = study)
    expect_equal(unique(result$episode_id), 3L)
  }
})

test_that("absorbing episodes allow later episodes by default and suppression is explicit", {
  path <- tempfile("c2t-episodes-absorbing-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- episode_rows("001", c("0", "0", "A", "Dead", "B"),
                        c("2020-01-01", "2020-02-01", "2020-01-01", "2020-01-08", "2020-02-01"),
                        c("2020-01-14", "2020-02-14", "2020-01-07", "2020-01-08", "2020-02-14"))
  for (type in c("Discrete", "Continuous")) {
    study <- episode_study(path, type, input, type, stateCohortAbsorbing = "Dead")
    expect_identical(study$stopAfterAbsorbingEpisode, FALSE)
    getDataForStudy(studyEnv = study)
    expected <- createTrajectories(studyEnv = study)
    expect_equal(unique(expected$episode_id), 1:2)
    expect_true("B" %in% expected$state_label[expected$episode_id == 2L])
    suppressed <- createTrajectories(studyEnv = study, stopAfterAbsorbingEpisode = TRUE)
    expect_equal(unique(suppressed$episode_id), 1L)
    expect_true("Dead" %in% suppressed$state_label)
    expect_equal(createTrajectories(studyEnv = study, stopAfterAbsorbingEpisode = FALSE), expected)

    # Dead is present but prohibited after A, so it must not suppress episode 2.
    rejected <- createTrajectories(
      studyEnv = study, stopAfterAbsorbingEpisode = TRUE,
      allowedStatesList = list(A = "A", B = "B", Dead = character(0))
    )
    expect_false("Dead" %in% rejected$state_label)
    expect_equal(unique(rejected$episode_id), 1:2)
    expect_equal(rejected$state_start_date[
      rejected$episode_id == 1L & rejected$state_label == "EXIT"
    ], as.Date("2020-01-15"))
  }
})

test_that("episode results preserve patient IDs across batches and saved replay", {
  path <- tempfile("c2t-episodes-replay-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- do.call(rbind, lapply(c("001", "9007199254740993", "NA"), function(id) {
    episode_rows(id, c("0", "0", "A", "B"),
                 c("2020-01-01", "2020-01-15", "2020-01-01", "2020-01-15"),
                 c("2020-01-07", "2020-01-21", "2020-01-07", "2020-01-21"))
  }))
  for (type in c("Discrete", "Continuous")) {
    study <- episode_study(path, type, input, type, batchSize = 1,
                           stopAfterAbsorbingEpisode = TRUE)
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_setequal(unique(result$subject_id), c("001", "9007199254740993", "NA"))
    expect_equal(createTrajectories(studyEnv = study, batchSize = 50), result)
    settings <- file.path(path, type, "Settings", "analysisSettings.rds")
    snapshot <- readRDS(settings)
    expect_identical(snapshot$version, 6L)
    expect_identical(snapshot$import$targetEpisodeHandling, "All")
    expect_identical(snapshot$generation$stopAfterAbsorbingEpisode, TRUE)

    newPath <- file.path(path, paste0(type, "-replay"))
    newSettings <- file.path(newPath, type, "Settings")
    dir.create(newSettings, recursive = TRUE)
    expect_true(file.copy(settings, newSettings))
    restored <- cohort2TrajectoryConfiguration(
      pathToStudy = newPath, studyName = type, useCDM = FALSE,
      runSavedStudy = TRUE, trajectoryDataObject = input
    )
    expect_identical(restored$targetEpisodeHandling, "All")
    expect_identical(restored$stopAfterAbsorbingEpisode, TRUE)
    getDataForStudy(studyEnv = restored)
    expect_equal(createTrajectories(studyEnv = restored), result)
    expect_error(createTrajectories(studyEnv = restored, stopAfterAbsorbingEpisode = FALSE),
                 "[Ss]aved")
  }
})

test_that("inclusive target overlaps are rejected while adjacent periods remain distinct", {
  path <- tempfile("c2t-episodes-overlap-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  overlapping <- episode_rows("001", c("0", "0"),
                              c("2020-01-01", "2020-01-07"),
                              c("2020-01-07", "2020-01-14"))
  study <- episode_study(path, "overlapping", overlapping, "Discrete")
  expect_error(getDataForStudy(studyEnv = study), "[Oo]verlap")
  adjacent <- overlapping
  adjacent$cohort_start_date[2] <- as.Date("2020-01-08")
  study <- episode_study(path, "adjacent", adjacent, "Discrete")
  getDataForStudy(studyEnv = study)
  result <- createTrajectories(studyEnv = study)
  expect_equal(result$episode_id[result$state_label == "START"], 1:2)
  expect_equal(result$state_start_date[result$state_label == "START"],
               as.Date(c("2020-01-01", "2020-01-08")))
  expect_error(episode_study(path, "outside", adjacent, "Discrete", outOfCohortAllowed = TRUE),
               "outOfCohortAllowed")
})

test_that("first-period behavior remains the default and changing import policy requires reimport", {
  path <- tempfile("c2t-episodes-first-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- episode_rows("001", c("0", "0", "A", "B"),
                        c("2020-01-01", "2020-01-15", "2020-01-01", "2020-01-15"),
                        c("2020-01-07", "2020-01-21", "2020-01-07", "2020-01-21"))
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = type, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = c("A", "B"),
      stateCohortPriorityOrder = c("B", "A"), stateSelectionType = "Priority",
      trajectoryType = type, cycleLength = 7
    )
    expect_identical(study$targetEpisodeHandling, "First")
    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_false("episode_id" %in% names(result))
    expect_equal(sum(result$state_label == "START"), 1L)
    expect_identical(attr(result, "trajectory_definition")$version, 1L)
    study$targetEpisodeHandling <- "All"
    expect_error(createTrajectories(studyEnv = study), "[Ii]mport")
    getDataForStudy(studyEnv = study, targetEpisodeHandling = "All")
    expect_identical(study$targetEpisodeHandling, "All")
    expect_equal(unique(createTrajectories(studyEnv = study)$episode_id), 1:2)
  }
})
