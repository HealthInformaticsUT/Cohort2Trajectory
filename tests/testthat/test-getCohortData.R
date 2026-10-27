#library(testthat)
#library(Eunomia)
#library(Cohort2Trajectory)
#library(dplyr)
# getCohortData <- function(cdm,
#                           selectedTarget,
#                           selectedStates,
#                           # customizedStates = NULL,
#                           baseUrl = "http://localhost:8080/WebAPI",
#                           pathToStudy = env$pathToStudy,
#                           studyName = env$studyName

# Function description step by step:
# if needed get json files from atlas
# generateCohortSetCSV for cdmConnector
# read cohortSet for subjectids and 
# read_cohort_set cdmConnector
# generate cohort set
# collect data 
# map generated IDs to clinical labels and target 0
# select data 

# test cases:
# getCohortData with value length and size check
# same but with target also as state
# 

test_that("CDM import keeps clinical labels separate from generated cohort names", {
  root <- tempfile("c2t-cohort-names-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  labels <- c("Recorded pneumonia", "target", "Recorded_pneumonia",
              "30-day recovery", "(Other care)", "\u00c4ge faas")
  definitions <- c("clinical", "target", rep("clinical", 4))
  study <- cohort2TrajectoryConfiguration(
    studyName = "names", pathToStudy = root, baseUrl = NULL,
    atlasTargetCohort = "target", atlasStateCohorts = definitions,
    stateCohortLabels = labels
  )
  file.copy(file.path("testStudyMain", "JSON", c("test1.json", "test3.json")),
            file.path(root, "names", "JSON", c("target.json", "clinical.json")))
  expect_error(generateCohortSetCSV(c("target", definitions), c(labels, "Extra"),
                                    root, "names"), "one label for each state cohort")
  dbPath <- file.path(root, "test.duckdb")
  file.copy(CDMConnector::eunomiaDir("GiBleed"), dbPath)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbPath)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cdm <- CDMConnector::cdmFromCon(con, cdmName = "test", cdmSchema = "main", writeSchema = "main")
  data <- getCohortData(cdm = cdm, studyEnv = study, selectedTarget = "target",
                        selectedStates = definitions, stateCohortLabels = labels,
                        baseUrl = NULL)

  expect_setequal(data$cohort_definition_id, c("0", labels))
  records <- function(label) {
    rows <- data[data$cohort_definition_id == label,
                 c("subject_id", "cohort_start_date", "cohort_end_date")]
    dplyr::arrange(rows, subject_id, cohort_start_date, cohort_end_date)
  }
  # Reusing a definition must not duplicate rows or lose either clinical label.
  expect_equal(records("0"), records("target"))
  expect_equal(records("Recorded pneumonia"), records("Recorded_pneumonia"))
  # Clinical labels are values, not SQL names or filenames.
  for (label in labels[4:6]) {
    expect_equal(records(label), records("Recorded pneumonia"))
  }
})

test_that("getCohortData result nrow and ncol check ", {
  
  DBIObject <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomiaDir("GiBleed"))
  cdm <- CDMConnector::cdmFromCon(con = DBIObject,
                                    cdmSchema = "main",
                                    writeSchema = "main")
  
  data <- getCohortData(
    cdm = cdm, 
    selectedTarget = "0",
    studyEnv = studyEnv,
    selectedStates = c("Test_State1", "Test_State2"), 
    stateCohortLabels = c("test_state1", "test_state2"),
    baseUrl = NULL, 
    pathToStudy = paste0(getwd()), 
    studyName = "testStudy")
  
  testthat::expect_equal(nrow(data), 7978)
  testthat::expect_equal(ncol(data), 4)
  DBI::dbDisconnect(DBIObject)
})

test_that("getCohortData result nrow and ncol check with target state and selected state", {
  
  DBIObject <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomiaDir("GiBleed"))
  cdm <- CDMConnector::cdmFromCon(con = DBIObject,
                                    cdmSchema = "main",
                                    writeSchema = "main")
  
  data <- getCohortData(
    cdm = cdm, 
    selectedTarget = "0", 
    studyEnv = studyEnv,
    selectedStates = c("0","Test_State1", "Test_State2"), 
    baseUrl = NULL, 
    pathToStudy = paste0(getwd()), 
    studyName = "testStudy",
    stateCohortLabels = c("target_in_states","test_state1", "test_state2"))
    
  
  testthat::expect_equal(nrow(data), 10664)
  testthat::expect_equal(ncol(data), 4)
  DBI::dbDisconnect(DBIObject)
})

test_that("getCohortData result nrow and ncol check with target state and selected state", {
  
  DBIObject <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomiaDir("GiBleed"))
  cdm <- CDMConnector::cdmFromCon(con = DBIObject,
                                    cdmSchema = "main",
                                    writeSchema = "main")
  
  data <- getCohortData(
    cdm = cdm, 
    selectedTarget = "1", 
    studyEnv = studyEnv,
    selectedStates = c("1","Test_State1", "Test_State2"), 
    baseUrl = NULL, 
    pathToStudy = paste0(getwd()), 
    studyName = "testStudy",
    stateCohortLabels = c("target_in_state","test_state1", "test_state2"))
    
  
  testthat::expect_equal(nrow(data), 10664)
  testthat::expect_equal(ncol(data), 4)
  DBI::dbDisconnect(DBIObject)
})


