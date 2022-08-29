library(testthat)
library(Eunomia)
library(Cohort2Trajectory)

test_that("Constructed trajectory data nrow is equal to expected (5532)", {
studyName <- "TestCohort2TrajectorySaved" # TODO
pathToResults <<-  dirname(getwd()) # TODO # default value: paste(getwd(), "/tmp", sep = "")

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver <- './Drivers'
dbms <- "sqlite" #TODO

cdmSchema <- "main" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <- "main" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <- "main" #TODO # Schema which will contain the final results

baseUrl <- NULL #TODO # WebAPI URL is not needed when jsons' are already imported


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

Cohort2Trajectory::Cohort2Trajectory(
  dbms = "sqlite",
  connection = conn,
  cdmSchema = cdmSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  runSavedStudy = TRUE,
  pathToResults = pathToResults
)



data <- readr::read_csv(paste(pathToResults,"/tmp/datasets/TestCohort2TrajectorySavedpatientDataPriority.csv", sep = ""))

  expect_equal(nrow(data), 63)
})
#> Test passed 🥇
