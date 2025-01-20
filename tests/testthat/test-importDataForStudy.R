library(testthat)
#library(Eunomia)
library(Cohort2Trajectory)
library(dplyr)

test_that("import data function with data as input", {
  db <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(db, cdm_name="eunomia", cdm_schema = "main", write_schema="main")
  
  cohort2TrajectoryConfiguration(
    studyName = "testStudy",
    pathToStudy = paste0(getwd()),
  )
  
  rawData <- read.csv(paste0(getwd(), "/testStudy/Data/importedData.csv"))
  
  getDataForStudy(runSavedStudy = FALSE,
                  useCDM = FALSE,
                  trajectoryDataObject = rawData,
                  cdm = cdm,
                  studyEnv = studyEnv,
                  baseUrl = NULL,
                  outOfCohortAllowed = FALSE,
                  stateCohortMandatory = c("TestState2"),
                  stateCohortPriorityOrder = c("TestState1","TestState2"),
                  mergeStates = FALSE)
  
  dataCleaned <- read.csv(paste0(getwd(),"/testStudy/Data/importedDataCleaned_.csv"))
  expect_equal(nrow(dataCleaned), 63)
  expect_equal(ncol(dataCleaned), 5)
  DBI::dbDisconnect(db)
})

test_that("import data function with path to data as input", {
  db <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(db, cdm_name="eunomia", cdm_schema = "main", write_schema="main")
  
  cohort2TrajectoryConfiguration(
    studyName = "testStudy",
    pathToStudy = paste0(getwd())
  )

  getDataForStudy(runSavedStudy = FALSE,
                  useCDM = FALSE,
                  trajectoryDataObject = NULL,
                  cdm = cdm,
                  studyEnv = studyEnv,
                  baseUrl = NULL,
                  outOfCohortAllowed = FALSE,
                  stateCohortMandatory = c("TestState2"),
                  stateCohortPriorityOrder = c("TestState1","TestState2"),
                  mergeStates = FALSE,
                  pathToData = paste0(getwd(), "/testStudy/Data/importedData.csv"))
  
  dataCleaned <- read.csv(paste0(getwd(),"/testStudy/Data/importedDataCleaned_.csv"))
  expect_equal(nrow(dataCleaned), 63)
  expect_equal(ncol(dataCleaned), 5)
  DBI::dbDisconnect(db)
})

