test_that("original text IDs survive public import, merging, batching and CSV output", {
  ids <- c("9007199254740992", "9007199254740993", "001", "1")
  raw <- data.frame(
    subject_id = ids[c(1, 1, 1, 2, 2, 3, 3, 4, 4)],
    cohort_definition_id = c("0", "A", "B", "0", "C", "0", "D", "0", "E"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 3, 0, 0, 0, 0, 0, 0),
    cohort_end_date = as.Date("2020-01-01") + c(6, 4, 6, 6, 6, 6, 6, 6, 6)
  )
  path <- tempfile("c2t-large-ids-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  inputFile <- file.path(path, "input.csv")
  write.csv(raw, inputFile, row.names = FALSE)
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = path, useCDM = FALSE, atlasTargetCohort = "0",
      stateCohortLabels = c("A", "B", "C", "D", "E", "A+B"),
      stateCohortPriorityOrder = c("A+B", "A", "B", "C", "D", "E"),
      stateCohortAbsorbing = "B", stateSelectionType = "Priority",
      trajectoryType = type, lengthOfStay = 1, runSavedStudy = TRUE
    )
    getDataForStudy(studyEnv = study, runSavedStudy = FALSE,
                    pathToData = inputFile, mergeStates = TRUE)
    cleaned <- read.csv(file.path(path, type, "Data", "importedDataCleaned_0.csv"),
                        colClasses = c(subject_id = "character"))
    expect_equal(cleaned$subject_id[cleaned$cohort_definition_id == "A+B"], ids[1])
    together <- createTrajectories(studyEnv = study, batchSize = 4)
    separate <- createTrajectories(studyEnv = study, batchSize = 1)
    expect_equal(separate, together)
    expect_equal(sort(unique(together$subject_id)), sort(ids))
    expect_true(all(together$state_label[together$subject_id == ids[2]] %in%
                      c("START", "C", "EXIT")))
    saved <- read.csv(file.path(path, type, "Data", paste0("patientData", type, ".csv")),
                      colClasses = c(subject_id = "character"))
    expect_equal(saved$subject_id, separate$subject_id)
  }
})

test_that("integer64 IDs are restored before exact demographic joins", {
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste(
    "CREATE TABLE person AS SELECT CAST('9007199254740992' AS BIGINT) person_id,",
    "8507 gender_concept_id, DATE '1960-01-01' birth_datetime UNION ALL SELECT",
    "CAST('9007199254740993' AS BIGINT), 8532, DATE '1970-01-01'"
  ))
  ids <- DBI::dbGetQuery(con, "SELECT person_id FROM person ORDER BY person_id")$person_id
  input <- data.frame(
    subject_id = ids[c(1, 1, 2, 2)], cohort_definition_id = c("0", "A", "0", "B"),
    cohort_start_date = as.Date("2020-01-01"), cohort_end_date = as.Date("2020-01-07")
  )
  cleaned <- cleanCohortData(input, mandatoryStates = NULL)
  expect_equal(as.character(unique(cleaned$subject_id)), as.character(ids))
  cdm <- list(person = dplyr::tbl(con, "person"))
  outputs <- list(
    getTrajectoriesDiscrete(cdm, cleaned, stateDuration = 7, statePriorityVector = c("A", "B")),
    getTrajectoriesContinuous(cdm, cleaned, stateSelection = 3, statePriorityVector = c("A", "B"))
  )
  for (output in outputs) {
    expect_s3_class(output$subject_id, "integer64")
    starts <- output[output$state_label == "START", ]
    expect_equal(as.character(starts$subject_id), as.character(ids))
    expect_equal(starts$gender_concept_id, c(8507, 8532))
  }
})

test_that("invalid patient IDs fail before they can mix histories", {
  for (id in list(NA_real_, Inf, 1.5, 2^53, "")) {
    data <- data.frame(subject_id = id, cohort_definition_id = "0",
                       cohort_start_date = as.Date("2020-01-01"),
                       cohort_end_date = as.Date("2020-01-07"))
    expect_error(cleanCohortData(data, mandatoryStates = NULL),
                 "subject_id")
    expect_error(getTrajectoriesContinuous(NULL, data, 3, NULL, addPersonalData = FALSE),
                 "subject_id")
  }
})
