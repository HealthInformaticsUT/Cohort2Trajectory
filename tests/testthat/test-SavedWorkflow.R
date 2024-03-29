library(testthat)
library(Eunomia)
library(Cohort2Trajectory)

test_that("Constructed trajectory data nrow is equal to expected (63)", {
studyName <- "TestCohort2TrajectorySaved" # TODO
pathToResults <<-  dirname(getwd()) # TODO # default value: paste(getwd(), "/tmp", sep = "")

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver <- './Drivers'

cdmSchema <- "main" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <- "main" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <- "main" #TODO # Schema which will contain the final results
cdmVocabSchema <- "main"

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

result <- Cohort2Trajectory::Cohort2Trajectory(
  dbms = connectionDetails$dbms,
  connection = conn,
  cdmSchema = cdmSchema,
  cdmVocabSchema = cdmVocabSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  runSavedStudy = TRUE,
  pathToResults = pathToResults
)



#data <- readr::read_csv(paste(pathToResults,"/tmp/datasets/TestCohort2TrajectorySavedpatientDataPriority.csv", sep = ""))

  expect_equal(result, NULL)
})
#> Test passed 🥇
