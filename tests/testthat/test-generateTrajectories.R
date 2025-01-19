testthat::test_that("Create trajectories discrete", {
  db <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(db, cdm_name="eunomia", cdm_schema = "main", write_schema="main")
  
  studyEnv <- cohort2TrajectoryConfiguration(
    studyName = "testStudy",
    pathToStudy = paste0(getwd()),
    atlasTargetCohort = "0"
  )
  assign("studyEnv", studyEnv, envir = globalenv())
  
  createTrajectories(
    cdm = cdm,
    trajectoryType = 0,
    runSavedStudy = FALSE,
    studyEnv = studyEnv,
    oocFix = "None",
    outOfCohortAllowed = FALSE,
    lengthOfStay = 30,
    stateCohortLabels = c("test_state1","test_state2"),
    stateCohortPriorityOrder = c("test_state1","test_state2"),
    stateCohortMandatory = c("test_state2"),
    stateCohortAbsorbing = c("test_state2"),
    stateSelectionType = 3,
    allowedStatesList = createStateList(c("test_state1","test_state2")),
    useCDM = TRUE)
  
  
  dataTrajectories <- read.csv(paste0(getwd(),"/testStudy/Data/patientDataDiscrete.csv"))
  expect_equal(ncol(dataTrajectories), 9)
  DBI::dbDisconnect(db)
})


testthat::test_that("Create trajectories continuous", {
  db <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
  cdm <- CDMConnector::cdm_from_con(db, cdm_name="eunomia", cdm_schema = "main", write_schema="main")
  
  studyEnv <- cohort2TrajectoryConfiguration(
    studyName = "testStudy",
    pathToStudy = paste0(getwd()),
    atlasTargetCohort = "0"
  )
  assign("studyEnv", studyEnv, envir = globalenv())
  
  createTrajectories(pathToStudy,
                     cdm = cdm,
                     trajectoryType = 1,
                     runSavedStudy = FALSE,
                     studyEnv = studyEnv,
                     oocFix = "None",
                     outOfCohortAllowed = FALSE,
                     lengthOfStay = 30,
                     stateCohortLabels = c("test_state1","test_state2"),
                     stateCohortPriorityOrder = c("test_state1","test_state2"),
                     stateCohortMandatory = c("test_state2"),
                     stateCohortAbsorbing = c("test_state2"),
                     stateSelectionType = 3,
                     allowedStatesList = createStateList(c("test_state1","test_state2")),
                     useCDM = TRUE)
  
  dataTrajectories <- read.csv(paste0(getwd(), "/testStudy/Data/patientDataContinuous.csv"))
  expect_equal(ncol(dataTrajectories), 9)
  DBI::dbDisconnect(db, shutdown = TRUE)
})

