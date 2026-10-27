test_that("a moved study restores its rules and reads a different CDM", {
  root <- tempfile("c2t-portable-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  sourcePath <- file.path(root, "source")
  destinationPath <- file.path(root, "destination")
  dir.create(sourcePath)
  dir.create(destinationPath)
  study <- cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = sourcePath, baseUrl = NULL,
    atlasTargetCohort = "0", atlasStateCohorts = c("first", "second"),
    stateCohortLabels = c("a", "b"), stateCohortPriorityOrder = c("b", "a"),
    stateCohortMandatory = "b", stateCohortAbsorbing = "b",
    stateSelectionType = "Priority", lengthOfStay = 14,
    oocFix = "Last present state", allowedStatesList = list(a = c("a", "b"), b = "b"),
    mergeThreshold = 0.75
  )
  file.copy(file.path("testStudyMain", "JSON", c("test1.json", "test3.json", "test4.json")),
            file.path(sourcePath, "portable", "JSON", c("0.json", "first.json", "second.json")))
  dbPaths <- file.path(root, c("source.duckdb", "destination.duckdb"))
  for (path in dbPaths) file.copy(CDMConnector::eunomiaDir("GiBleed"), path)
  con1 <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbPaths[1])
  on.exit(DBI::dbDisconnect(con1, shutdown = TRUE), add = TRUE)
  cdm1 <- CDMConnector::cdmFromCon(con1, cdmName = "source", cdmSchema = "main", writeSchema = "main")
  getDataForStudy(cdm = cdm1, studyEnv = study)
  expected <- createTrajectories(cdm = cdm1, studyEnv = study)

  dir.create(file.path(destinationPath, "portable"))
  file.copy(file.path(sourcePath, "portable", c("JSON", "Settings")),
            file.path(destinationPath, "portable"), recursive = TRUE)
  restored <- cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = destinationPath, runSavedStudy = TRUE, batchSize = 5
  )
  expect_equal(restored$allowedStatesList, study$allowedStatesList)
  expect_equal(c(restored$lengthOfStay, restored$mergeThreshold), c(14, 0.75))
  expect_error(createTrajectories(studyEnv = restored), "getDataForStudy successfully first")
  expect_error(cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = destinationPath, runSavedStudy = TRUE, lengthOfStay = 7
  ), "Do not also supply: lengthOfStay")

  con2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbPaths[2])
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  # A site-specific difference verifies that output is actually read afresh.
  DBI::dbExecute(con2, "UPDATE person SET gender_concept_id = 8507")
  cdm2 <- CDMConnector::cdmFromCon(con2, cdmName = "destination", cdmSchema = "main", writeSchema = "main")
  getDataForStudy(cdm = cdm2, studyEnv = restored)
  actual <- createTrajectories(cdm = cdm2, studyEnv = restored)
  expect_true(all(actual$gender_concept_id == 8507))
  expected$gender_concept_id <- actual$gender_concept_id
  expect_equal(dplyr::arrange(actual, subject_id, time_in_cohort, state_label),
                dplyr::arrange(expected, subject_id, time_in_cohort, state_label))

  unlink(file.path(destinationPath, "portable", "JSON", "0.json"))
  expect_error(getDataForStudy(cdm = cdm2, studyEnv = restored), "Missing saved cohort definition.*0.json")
  expect_error(createTrajectories(cdm = cdm2, studyEnv = restored), "getDataForStudy successfully first")
  unlink(file.path(destinationPath, "portable", "Settings", "analysisSettings.rds"))
  expect_error(cohort2TrajectoryConfiguration(
    studyName = "portable", pathToStudy = destinationPath, runSavedStudy = TRUE
  ), "Older CSV settings did not save")
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
      stateSelectionType = "Priority", trajectoryType = type, lengthOfStay = 7,
      allowedStatesList = list(), stateCohortAbsorbing = NULL,
      mergeStates = TRUE, mergeThreshold = 0.7
    )
    getDataForStudy(studyEnv = study)
    expected <- createTrajectories(studyEnv = study, updateAutomaticallyForMergedStates = TRUE)
    inputFile <- file.path(root, paste0(type, ".csv"))
    write.csv(input, inputFile, row.names = FALSE)
    restored <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = root, runSavedStudy = TRUE, useCDM = FALSE,
      pathToData = inputFile
    )
    getDataForStudy(studyEnv = restored)
    expect_equal(createTrajectories(studyEnv = restored), expected)
    expect_true(restored$mergeStates)
    expect_true(restored$updateAutomaticallyForMergedStates)
    expect_length(restored$allowedStatesList, 0)
    expect_length(restored$stateCohortAbsorbing, 0)
  }
})
