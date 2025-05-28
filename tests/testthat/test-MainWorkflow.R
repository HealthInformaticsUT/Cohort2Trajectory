library(testthat)
#library(Eunomia)
library(Cohort2Trajectory)

test_that("Imported data nrow is equal to expected (63)", {
  ################################################################################
  #
  # Initiate the database connection
  #
  ################################################################################
  db <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir("GiBleed"))
  cdm <- CDMConnector::cdmFromCon(
    db,
    cdmName = "eunomia",
    cdmSchema = "main",
    writeSchema = "main"
  )
  
  
  studyEnv <- cohort2TrajectoryConfiguration(
    baseUrl = NULL,
    studyName = "testStudyMain",
    pathToStudy = paste0(getwd()), #paste0(getwd(), "/tests/testthat/"), #
    atlasTargetCohort = "test1",
    atlasStateCohorts = c("test3", "test4"),
    stateCohortLabels = c("test_state1", "test_state2"),
    stateCohortMandatory = c("test_state2"),
    stateCohortAbsorbing = c("test_state2"),
    outOfCohortAllowed = FALSE,
    trajectoryType = "Discrete",
    lengthOfStay = 30,
    stateSelectionType = "Priority",
    stateCohortPriorityOrder = c("test_state1", "test_state2"),
    runSavedStudy = FALSE,
    batchSize = 10
  )
  
  assign("studyEnv", studyEnv, envir = globalenv())
  
  # get data
  getDataForStudy(cdm = cdm,studyEnv = studyEnv)
  
  # create trajectories
  createTrajectories(cdm = cdm, runSavedStudy = F, studyEnv = studyEnv)
  
  data <- read.csv(
    paste(
      studyEnv$pathToStudy,
      "/",
      studyEnv$studyName,
      "/Data/importedDataCleaned_",
      studyEnv$atlasTargetCohort,
      ".csv",
      sep = ""
    )
  )
  
  expect_equal(nrow(data), 63)
  DBI::dbDisconnect(db)
})


test_that("Constructed trajectory data nrow is equal to expected (84)", {
  cohort2TrajectoryConfiguration(
    baseUrl = NULL,
    studyName = "testStudyMain",
    pathToStudy = paste0(getwd()),
  )
  
  data <- read.csv(
    paste(
      studyEnv$pathToStudy,
      "/",
      studyEnv$studyName,
      "/Data/",
      "patientDataDiscrete.csv",
      sep = ""
    )
  )
  
  expect_equal(nrow(data), 84)
})
