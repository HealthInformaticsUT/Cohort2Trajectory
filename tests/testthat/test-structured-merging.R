test_that("composition resolves exact bounded overlaps without parsing labels", {
  labels <- c("A", "B", "A + B", "Care [x]")
  raw <- data.frame(subject_id = c(rep("001", 5), "002"),
                    cohort_definition_id = c("0", labels, "0"),
                    cohort_start_date = as.Date("2020-01-01") + c(0, -2, 2, 8, 3, 0),
                    cohort_end_date = as.Date("2020-01-01") + c(9, 5, 7, 12, 4, 9))
  cleaned <- cleanCohortData(raw, mandatoryStates = NULL)
  merged <- .composeStateIntervals(cleaned, labels, list())
  clinical <- merged$data[merged$data$cohort_definition_id != "0", ]
  expected <- list("A", c("A", "B"), c("A", "B", "Care [x]"),
                   c("A", "B"), "B", "A + B")
  expect_equal(clinical$cohort_definition_id, vapply(expected, .stateKey, character(1)))
  expect_equal(as.numeric(clinical$cohort_start_date - as.Date("2020-01-01")), c(0, 2, 3, 5, 6, 8))
  expect_equal(as.numeric(clinical$cohort_end_date - as.Date("2020-01-01")), c(1, 2, 4, 5, 7, 9))
  expect_false(anyDuplicated(merged$registry$label) > 0L)
  expect_false(.stateKey(c("A", "B")) == .stateKey("A + B"))
  expect_equal(merged$data$subject_id[merged$data$cohort_definition_id == "0"], c("001", "002"))
  reordered <- .composeStateIntervals(cleaned[nrow(cleaned):1L, ], rev(labels), list())
  expect_equal(reordered$registry, merged$registry)
})

test_that("union inheritance and explicit replacements use structured membership", {
  labels <- c("A", "B", "C", "D")
  registry <- .stateRegistry(labels, list(c("A", "B"), c("C", "D")))
  rules <- list(A = c("A", "C"), B = character(0), C = "C", D = "D")
  result <- .mergedRules(registry, labels, rules, "A", list())
  ab <- .stateKey(c("A", "B"))
  cd <- .stateKey(c("C", "D"))
  expect_true(.stateKey("C") %in% result$allowed[[ab]])
  expect_false(cd %in% result$allowed[[ab]])
  expect_true(ab %in% result$absorbing)
  expect_length(result$allowed[[.stateKey("B")]], 0)
  custom <- list(mergedState(c("B", "A"), allowedTransitions = list(c("C", "D")),
                            absorbing = FALSE, label = "Custom + label [x]"))
  changed <- .mergedRules(registry, labels, rules, "A", custom)
  expect_identical(changed$allowed[[ab]], cd)
  expect_false(ab %in% changed$absorbing)
  expect_identical(changed$registry$label[changed$registry$state_key == ab], "Custom + label [x]")
  empty <- .mergedRules(registry, labels, rules, "A",
                         list(mergedState(c("A", "B"), allowedTransitions = character(0))))
  expect_length(empty$allowed[[ab]], 0)
  expect_true(ab %in% empty$absorbing)
  onlyFlag <- .mergedRules(registry, labels, rules, "A",
                            list(mergedState(c("A", "B"), absorbing = FALSE)))
  expect_identical(onlyFlag$allowed, result$allowed)
  expect_error(mergedState(c("A", "B"), absorbing = NA), "TRUE or FALSE")
  expect_error(.stateRegistry(labels, overrides = list(mergedState(c("A", "unknown")))), "Unknown state members")
  expect_error(.validateMergeOptions(TRUE, rep(custom, 2)), "Only one override")
})

test_that("merged overrides survive batching and a fresh saved-study import", {
  root <- tempfile("c2t-structured-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  labels <- c("A", "B", "C")
  raw <- data.frame(subject_id = rep(c("001", "002"), each = 4),
                    cohort_definition_id = rep(c("0", "A", "B", "C"), 2),
                    cohort_start_date = as.Date("2020-01-01") + rep(c(0, 0, 0, 7), 2),
                    cohort_end_date = as.Date("2020-01-01") + rep(c(13, 6, 6, 13), 2))
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = root, studyName = type, useCDM = FALSE, trajectoryDataObject = raw,
      stateCohortLabels = labels, stateCohortPriorityOrder = labels,
      stateCohortMandatory = "B", stateCohortAbsorbing = "A",
      stateSelectionType = "Priority", trajectoryType = type, lengthOfStay = 7,
      allowedStatesList = list(A = "C", B = character(0), C = "C"), mergeStates = TRUE,
      mergedStateOverrides = list(mergedState(c("A", "B"), absorbing = FALSE, label = "Combined"))
    )
    getDataForStudy(studyEnv = study)
    source <- readRDS(file.path(root, type, "Data", "sourceIntervals.rds"))
    expect_setequal(unique(source$cohort_definition_id), c("0", labels))
    together <- createTrajectories(studyEnv = study, batchSize = 2)
    expect_identical(together$state_label, rep(c("START", "Combined", "C", "EXIT"), 2))
    expect_equal(createTrajectories(studyEnv = study, batchSize = 1), together)
    saved <- readRDS(file.path(root, type, "Settings", "analysisSettings.rds"))
    expect_identical(saved$version, 4L)
    expect_identical(saved$generation$allowedStatesList, study$allowedStatesList)
    restored <- cohort2TrajectoryConfiguration(
      pathToStudy = root, studyName = type, useCDM = FALSE, runSavedStudy = TRUE,
      trajectoryDataObject = raw[raw$subject_id == "002", ]
    )
    getDataForStudy(studyEnv = restored)
    actual <- createTrajectories(studyEnv = restored)
    expect_equal(actual$state_key, together$state_key[together$subject_id == "002"])
    expect_identical(actual$state_label, c("START", "Combined", "C", "EXIT"))
    expect_equal(readRDS(file.path(root, type, "Data", "stateDictionary.rds")), restored$stateDictionary)
    # With no override, the absorbing member ends the trajectory.
    inherited <- createTrajectories(studyEnv = study, mergedStateOverrides = list())
    expect_false("C" %in% inherited$state_label)
  }
})

test_that("removed merging arguments are rejected", {
  expect_error(cohort2TrajectoryConfiguration(mergeThreshold = 0.9),
                 "unused argument.*mergeThreshold")
  expect_error(getDataForStudy(mergeThreshold = 0.9),
                 "unused argument.*mergeThreshold")
  expect_error(createTrajectories(updateAutomaticallyForMergedStates = FALSE),
                 "unused argument.*updateAutomaticallyForMergedStates")
})

test_that("older snapshot formats are rejected without modifying their files", {
  root <- tempfile("c2t-snapshot-version-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, useCDM = FALSE,
    stateCohortLabels = "A", stateSelectionType = "Priority", lengthOfStay = 7
  )
  snapshot <- list(import = mget(.studyImportFields(), envir = study),
                   generation = mget(.studyGenerationFields(), envir = study))
  file <- file.path(root, study$studyName, "Settings", "analysisSettings.rds")
  for (version in 1:3) {
    snapshot$version <- version
    saveRDS(snapshot, file)
    before <- tools::md5sum(file)
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = root, studyName = study$studyName, useCDM = FALSE, runSavedStudy = TRUE
    ), "Only version 4 is supported")
    expect_identical(tools::md5sum(file), before)
  }
})

test_that("merged labels do not override gap controls or named fallbacks", {
  root <- tempfile("c2t-merged-gaps-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  labels <- c("None", "Last present state", "A", "B")
  raw <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", labels),
    cohort_start_date = as.Date("2020-01-01") + c(0, 7, 14, 21, 21),
    cohort_end_date = as.Date("2020-01-01") + c(34, 13, 20, 27, 27)
  )
  # A declared literal label may look like another state's internal key.
  labels <- c(labels, .stateKey("A"))
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, studyName = "gaps", useCDM = FALSE,
    trajectoryDataObject = raw, stateCohortLabels = labels,
    stateCohortPriorityOrder = labels, stateSelectionType = "Priority",
    lengthOfStay = 7, mergeStates = TRUE,
    mergedStateOverrides = list(mergedState(c("A", "B"), label = "Combined"))
  )
  getDataForStudy(studyEnv = study)
  observed <- c("None", "Last present state", "Combined")
  expect_identical(createTrajectories(studyEnv = study)$state_label,
                   c("START", "OUT OF COHORT", observed, "OUT OF COHORT", "EXIT"))
  expect_identical(createTrajectories(studyEnv = study, oocFix = "Last present state")$state_label,
                   c("START", "OUT OF COHORT", observed, "Combined", "EXIT"))
  for (fallback in c("A", "Combined", .stateKey("A"))) {
    expect_identical(createTrajectories(studyEnv = study, oocFix = fallback)$state_label,
                     c("START", fallback, observed, fallback, "EXIT"))
  }
  expect_error(createTrajectories(studyEnv = study, oocFix = "combined"), "oocFix.*Combined")
  files <- file.path(root, "gaps", c("Data/patientDataDiscrete.csv", "Data/stateDictionary.rds",
                                     "Settings/analysisSettings.rds"))
  before <- tools::md5sum(files)
  for (key in c(.stateKey("B"), .stateKey(c("A", "B")))) {
    expect_error(createTrajectories(studyEnv = study, oocFix = key), "oocFix.*internal state key")
  }
  expect_identical(tools::md5sum(files), before)
})
