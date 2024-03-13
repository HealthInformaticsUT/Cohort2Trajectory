library(testthat)
library(Eunomia)
library(Cohort2Trajectory)

test_that("Imported data nrow is equal to expected (5017)", {
studyName = "TestCohort2Trajectory"
pathToResults <<- dirname(dirname(getwd())) #

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver <- './Drivers'

cdmSchema <- "main"  # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <- "main" # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <- "main" # Schema which will contain the final results
cdmVocabSchema <- "main"

baseUrl <- NULL  # WebAPI URL is not needed when jsons' are already imported


################################################################################
#
# Initiate the database connection
#
################################################################################
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
conn <- DatabaseConnector::connect(connectionDetails)
Eunomia::createCohorts(connectionDetails)


################################################################################
#
# Run the study
#
################################################################################

Cohort2Trajectory(
  dbms = connectionDetails$dbms,
  connection = conn,
  cdmSchema = cdmSchema,
  cdmVocabSchema = cdmVocabSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  baseUrl = baseUrl,
  atlasTargetCohort = 1,
  atlasStateCohorts = c(3,4),
  stateCohortLabels = c("TestState1", "TestState2"),
  stateCohortPriorityOrder = c("TestState1", "TestState2"),
  stateCohortMandatory = c("TestState2"),
  stateCohortAbsorbing = c("TestState2"),
  ##############################################################################
  # stateSelectionTypes
  # 1 - First occurring
  # 2 - Largest overlap
  # 3 - Priority ordering
  ##############################################################################
  stateSelectionType = 3,
  ##############################################################################
  # trajectoryType
  # 0 - Discrete time
  # 1 - Continuous time
  ##############################################################################
  trajectoryType = 0,
  lengthOfStay = 30,
  outOfCohortAllowed = TRUE,
  runSavedStudy = FALSE,
  pathToResults = pathToResults
)


data <- readr::read_csv(paste(pathToResults,"/tmp/datasets/TestCohort2TrajectoryimportedData.csv", sep =""))

  expect_equal(nrow(data), 5017)
})
#> Test passed 🥇

test_that("Constructed trajectory data nrow is equal to expected (5532)", {
data <- readr::read_csv(paste(pathToResults,"/tmp/datasets/TestCohort2TrajectorypatientDataDiscrete.csv", sep = ""))

  expect_equal(nrow(data), 5532)
})
#> Test passed 🥇

