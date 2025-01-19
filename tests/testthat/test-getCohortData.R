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
# rename definition id
# filter if target given as state cohort tmpData
# set target 0
# rbind tmpData and data with target 0
# select data 

# test cases:
# getCohortData with value length and size check
# same but with target also as state
# 

test_that("getCohortData result nrow and ncol check ", {
  
  DBIObject <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(con = DBIObject,
                                    cdm_schema = "main",
                                    write_schema = "main")
  
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
  
  DBIObject <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(con = DBIObject,
                                    cdm_schema = "main",
                                    write_schema = "main")
  
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
  
  DBIObject <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(con = DBIObject,
                                    cdm_schema = "main",
                                    write_schema = "main")
  
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


