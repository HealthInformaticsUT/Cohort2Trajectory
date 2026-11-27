test_that("a moved study preserves nested definitions and clinical labels on a different CDM", {
  root <- tempfile("c2t-portable-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  sourcePath <- file.path(root, "source")
  destinationPath <- file.path(root, "destination")
  dir.create(sourcePath)
  dir.create(destinationPath)
  study <- cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = sourcePath, baseUrl = NULL,
    atlasTargetCohort = "0",
    # Clinical names differ from filenames and stay the same after restoring.
    atlasStateCohorts = c(second = "first", first = "states/second"),
    stateCohortPriorityOrder = c("first", "second"),
    stateCohortMandatory = "first", stateCohortAbsorbing = "first",
    stateSelectionType = "Priority", cycleLength = 14,
    oocFix = "Last present state", allowedStatesList = list(second = c("second", "first"), first = "first")
  )
  dir.create(file.path(sourcePath, "portable", "JSON", "states"))
  writeFixtureDefinitions(file.path(sourcePath, "portable", "JSON",
    c("0.json", "first.json", "states/second.json")))
  con1 <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con1, shutdown = TRUE), add = TRUE)
  cdm1 <- fixtureCdm(con1, "source")
  getDataForStudy(cdm = cdm1, studyEnv = study)
  expected <- createTrajectories(cdm = cdm1, studyEnv = study)

  dir.create(file.path(destinationPath, "portable"))
  file.copy(file.path(sourcePath, "portable", "Settings"),
            file.path(destinationPath, "portable"), recursive = TRUE)
  restored <- cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = destinationPath, runSavedStudy = TRUE, batchSize = 5
  )
  expect_equal(restored$allowedStatesList, study$allowedStatesList)
  expect_equal(restored$cycleLength, 14)
  expect_error(createTrajectories(studyEnv = restored), "getDataForStudy successfully first")
  expect_error(cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = destinationPath, runSavedStudy = TRUE, cycleLength = 7
  ), "Do not also supply: cycleLength")

  con2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  cdm2 <- fixtureCdm(con2, "destination")
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  # A site-specific difference verifies that output is actually read afresh.
  DBI::dbExecute(con2, "UPDATE person SET gender_concept_id = 8507")
  cdm2 <- CDMConnector::cdmFromCon(con2, cdmName = "destination", cdmSchema = "main", writeSchema = "main")
  getDataForStudy(cdm = cdm2, studyEnv = restored)
  actual <- createTrajectories(cdm = cdm2, studyEnv = restored)
  expect_true(all(actual$gender_concept_id == 8507))
  expected$gender_concept_id <- 8507L
  expect_equal(dplyr::arrange(actual, subject_id, time_in_cohort, state_label),
                dplyr::arrange(expected, subject_id, time_in_cohort, state_label))

  # Current CDM snapshots must contain the definitions from their completed run.
  restored$.savedSettings$definitionContents <- NULL
  expect_error(getDataForStudy(cdm = cdm2, studyEnv = restored), "do not contain the cohort definitions")
  expect_error(createTrajectories(cdm = cdm2, studyEnv = restored), "getDataForStudy successfully first")
  unlink(file.path(destinationPath, "portable", "Settings", "analysisSettings.rds"))
  expect_error(cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = destinationPath, runSavedStudy = TRUE
  ), "No complete saved-study settings found")
})

test_that("downloaded definitions remain portable independently of clinical labels", {
  root <- tempfile("c2t-atlas-portable-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  sourcePath <- file.path(root, "source")
  destinationPath <- file.path(root, "destination")
  dir.create(sourcePath)
  dir.create(destinationPath)
  expressions <- lapply(c(1001L, 2001L, 3001L), fixtureExpression)
  names(expressions) <- c("101", "202", "0")
  allowDownloads <- TRUE
  local_mocked_bindings(
    .getCohortDefinition = function(cohortId, baseUrl, headers = NULL) {
      if (!allowDownloads) stop("A restored study must not contact ATLAS.")
      list(expression = expressions[[as.character(cohortId)]])
    }, .package = "Cohort2Trajectory"
  )
  labels <- c("Swallowing / feeding", "Related care", "Airway support")
  study <- cohort2TrajectoryConfiguration(
    studyName = "atlas", pathToStudy = sourcePath, baseUrl = "https://atlas.example/WebAPI",
    atlasTargetCohort = 101,
    atlasStateCohorts = setNames(c("0202", "202", "0"), labels),
    stateCohortPriorityOrder = labels,
    stateCohortMandatory = labels[1], stateSelectionType = "Priority", cycleLength = 7
  )
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cdm <- fixtureCdm(con)
  getDataForStudy(cdm = cdm, studyEnv = study)
  expected <- createTrajectories(cdm = cdm, studyEnv = study)
  jsonPath <- file.path(sourcePath, "atlas", "JSON")
  expect_equal(jsonlite::fromJSON(file.path(jsonPath, "0.json"), simplifyVector = FALSE), expressions[["101"]])
  manifest <- read.csv(file.path(jsonPath, "CohortsToCreate.csv"))
  expect_true(all(file.exists(file.path(jsonPath, manifest$jsonPath))))
  snapshot <- readRDS(file.path(sourcePath, "atlas", "Settings", "analysisSettings.rds"))
  expect_equal(snapshot$definitions, list(target = "0", states = c("atlas_202", "atlas_202", "atlas_0")))

  dir.create(file.path(destinationPath, "atlas"))
  file.copy(file.path(sourcePath, "atlas", c("JSON", "Settings")),
            file.path(destinationPath, "atlas"), recursive = TRUE)
  allowDownloads <- FALSE
  restored <- cohort2TrajectoryConfiguration(
    studyName = "atlas", pathToStudy = destinationPath, runSavedStudy = TRUE
  )
  expect_equal(restored$stateCohortLabels, labels)
  getDataForStudy(cdm = cdm, studyEnv = restored)
  actual <- createTrajectories(cdm = cdm, studyEnv = restored)
  expect_equal(dplyr::arrange(actual, subject_id, time_in_cohort, state_label),
                dplyr::arrange(expected, subject_id, time_in_cohort, state_label))
})

test_that("saved local studies preserve merging and optional rules in both formats", {
  root <- tempfile("c2t-portable-local-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  input <- data.frame(subject_id = rep("001", 3), cohort_definition_id = c("0", "A", "B"),
                       cohort_start_date = as.Date("2020-01-01"),
                       cohort_end_date = as.Date("2020-01-14"))
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, useCDM = FALSE, trajectoryDataObject = input,
      stateCohortLabels = c("A", "B"), stateCohortPriorityOrder = c("A", "B"),
      stateSelectionType = "Priority", trajectoryType = type, cycleLength = 7,
      allowedStatesList = list(), stateCohortAbsorbing = NULL,
      mergeStates = TRUE
    )
    getDataForStudy(studyEnv = study)
    expected <- createTrajectories(studyEnv = study)
    inputFile <- file.path(root, paste0(type, ".csv"))
    write.csv(input, inputFile, row.names = FALSE)
    restored <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, runSavedStudy = TRUE, useCDM = FALSE,
      pathToData = inputFile
    )
    getDataForStudy(studyEnv = restored)
    expect_equal(createTrajectories(studyEnv = restored), expected)
    expect_true(restored$mergeStates)
    expect_equal(restored$stateDictionary, study$stateDictionary)
    expect_length(restored$allowedStatesList, 0)
    expect_length(restored$stateCohortAbsorbing, 0)
  }
})


test_that("version 4 cycle settings migrate in memory without changing portable settings", {
  root <- tempfile("c2t-version-four-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  study <- cohort2TrajectoryConfiguration(
    studyName = "old", pathToStudy = root, useCDM = FALSE,
    stateCohortLabels = c("A", "B"), stateCohortPriorityOrder = c("A", "B"),
    stateSelectionType = "Priority", cycleLength = 7,
    allowedStatesList = list(A = c("A", "B"), B = character(0))
  )
  generation <- mget(.studyGenerationFields(), envir = study)
  legacy <- generation
  legacy$stopAfterAbsorbingEpisode <- NULL
  names(legacy)[names(legacy) == "cycleLength"] <- "lengthOfStay"
  imported <- mget(.studyImportFields(), envir = study)
  imported$targetEpisodeHandling <- NULL
  snapshot <- list(version = 4L, import = imported,
                   generation = legacy, definitions = NULL, definitionContents = NULL,
                   stateDictionary = NULL)
  file <- file.path(root, "old", "Settings", "analysisSettings.rds")
  saveRDS(snapshot, file)
  before <- tools::md5sum(file)
  restored <- cohort2TrajectoryConfiguration(
    studyName = "old", pathToStudy = root, useCDM = FALSE, runSavedStudy = TRUE
  )
  expect_equal(restored$cycleLength, 7)
  expect_false(exists("lengthOfStay", envir = restored, inherits = FALSE))
  expected <- snapshot
  expected$version <- 6L
  expected$import$targetEpisodeHandling <- "First"
  expected$generation <- generation
  expect_identical(restored$.savedSettings, expected)
  expect_identical(tools::md5sum(file), before)

  # Old snapshots cannot combine, duplicate or omit the cycle setting.
  malformed <- list(
    c(legacy, list(cycleLength = 7)),
    c(legacy, list(lengthOfStay = 7)),
    legacy[names(legacy) != "lengthOfStay"]
  )
  for (fields in malformed) {
    invalid <- snapshot
    invalid$generation <- fields
    saveRDS(invalid, file)
    expect_error(.readStudySnapshot(root, "old"), "saved-study")
  }
  snapshot$generation$lengthOfStay <- 0
  saveRDS(snapshot, file)
  expect_error(.readStudySnapshot(root, "old"), "cycleLength must be a single positive whole number")

  # Version 5 accepts only the current field name.
  malformed <- snapshot
  malformed$version <- 5L
  malformed$generation <- legacy
  names(malformed$generation)[names(malformed$generation) == "lengthOfStay"] <- "cycleLength"
  malformed$generation$lengthOfStay <- 7
  saveRDS(malformed, file)
  expect_error(.readStudySnapshot(root, "old"), "Unsupported or incomplete saved-study")
})
