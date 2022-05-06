################################################################################
#
# Example on Eunomia
#
################################################################################

studyName = "TestCohort2Trajectory" #TODO
pathToResults <<-getwd() #TODO

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


################################################################################
#
# Run the study
#
################################################################################

Cohort2Trajectory::Cohort2Trajectory(
  dbms = dbms,
  connection = conn,
  cdmSchema = cdmSchema,
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