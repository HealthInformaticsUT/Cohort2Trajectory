################################################################################
#
# Example on Eunomia
#
################################################################################

studyName = "TestCohort2Trajectory" # TODO
pathToResults <- getwd() # TODO # default value: paste(getwd(), "/tmp", sep = "")

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver = './Drivers'
dbms <- "sqlite" #TODO

cdmSchema = "main" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema = "main" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema = "main" #TODO # Schema which will contain the final results

baseUrl = NULL #TODO # WebAPI URL is not needed when jsons' are already imported


################################################################################
#
# Initiate the database connection
#
################################################################################
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
conn <- DatabaseConnector::connect(connectionDetails)
Eunomia::createCohorts(connectionDetails)

Cohort2Trajectory::runGUI(
  conn,
  connectionDetails,
  pathToDriver = pathToDriver,
  pathToResults = pathToResults,
  dbms = dbms,
  cdmSchema = cdmSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  baseUrl = baseUrl
)