################################################################################
#
# Study settings
#
################################################################################
# devtools::install_github("HealthInformaticsUT/Cohort2Trajectory@v1.0.0") # Run for installing release v1.0.0
# devtools::install_github("HealthInformaticsUT/Cohort2Trajectory) # Run for installing the HEAD
library(Cohort2Trajectory)
studyName <- "HeartFailure" # TODO
pathToResults <- getwd()   # TODO

################################################################################
#
# Database credentials
#
################################################################################
pathToDriver <- './Drivers'
dbms <- "postgresql" #TODO
user <- '' #TODO
pw <- "" #TODO
server <- 'ip/database' #TODO
port <- '5432' #TODO

cdmSchema <- "ohdsi_cdm" #TODO # Schema which contains the OHDSI Common Data Model
cdmTmpSchema <- "ohdsi_temp" #TODO # Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmResultsSchema <- "ohdsi_results" #TODO # Schema which will contain the final results

baseUrl <- "http://localhost:8080/WebAPI" #TODO # WebAPI URL is not needed when jsons' are already imported
################################################################################
#
# Initiate the database connection
#
################################################################################

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = pw,
    port = port,
    pathToDriver = pathToDriver
  )

conn <- DatabaseConnector::connect(connectionDetails)

################################################################################
#
# Start the GUI application
#
################################################################################

runGUI(
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

################################################################################
#
# Customizing the allowedStatesList, which describes which transitions are
# possible
#
################################################################################

# stateCohortLabels = c("HF0", "HF1", "HF2", "HF3", "HFD")
# allowedStatesList = createStateList(stateCohortLabels) # Creates a list allowing all transitions from each state
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HF0", transitionTail = "HF2") # removes possibility to move from state4 to state2
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HF0", transitionTail = "HF3") # removes possibility to move from state2 to state3
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HF1", transitionTail = "HF3") # removes possibility to move from state4 to state2
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HF2", transitionTail = "HF0") # removes possibility to move from state2 to state3
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HF3", transitionTail = "HF1") # removes possibility to move from state4 to state2
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HF3", transitionTail = "HF0") # removes possibility to move from state2 to state3
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF1") # removes possibility to move from state4 to state2
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF0") # removes possibility to move from state2 to state3
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF3") # removes possibility to move from state4 to state2
# allowedStatesList = removeListVectorEl(stateList = allowedStatesList, transitionHead = "HFD", transitionTail = "HF2") # removes possibility to move from state2 to state3



# /.../
################################################################################
#
# Create the trajectories without using GUI
#
################################################################################

Cohort2Trajectory(
  dbms = dbms,
  connection = conn,
  cdmSchema = cdmSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  baseUrl = baseUrl,
  atlasTargetCohort = 1, # Target cohort id from ATLAS
  atlasStateCohorts = c(2,3,4,5,6), # State cohorts' ids from ATLAS
  stateCohortLabels = c("HF1", "HF0", "HFD","HF2","HF3"), # Customized labels in import order
  stateCohortPriorityOrder = c("HFD","HF3","HF2","HF1","HF0"), # Priority order of states
  stateCohortMandatory = c("HFD"), # Mandatory states
  stateCohortAbsorbing = c("HFD"), # Absorbing states
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
  pathToResults = pathToResults,
  allowedStatesList = allowedStatesList,
  ##############################################################################
  # oocFix (out of cohort fix)
  # None - out of cohort state is included
  # Last present state - out of cohort state is substituted with the latest state
  # Some state name - out of cohort state is substituted with some state's notation
  ##############################################################################
  oocFix = "Last present state"
)

################################################################################
#
# Create the trajectories with saved settings
#
################################################################################

Cohort2Trajectory(
  dbms = dbms,
  connection = conn,
  cdmSchema = cdmSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema,
  studyName = studyName,
  runSavedStudy = TRUE,
  pathToResults = pathToResults,
  allowedStatesList = allowedStatesList
)

################################################################################
#
# Load data and create trajectories without OMOP CDM (No connection)
#
# The imported .csv file has to have 4 columns:
# SUBJECT_ID (unique for each patient)
# COHORT_DEFINITION_ID (the target cohort has to be notated as "0". Other, state cohorts, can be notated in any way convenient (names should make sense).)
# COHORT_START_DATE (The start date of the corresponding cohort)
# COHORT_END_DATE (The end date of the corresponding cohort)
#
# Example of a patient data:
# "SUBJECT_ID","COHORT_DEFINITION_ID","COHORT_START_DATE","COHORT_END_DATE"
# 3141,"0",2020-02-22,2021-02-23
# 3150,"State1",2020-11-30,2020-12-01
# 3150,"State2",2021-01-01,2021-02-01
# ...
#
################################################################################

# Cohort2Trajectory(
#   studyName = studyName,
#   stateCohortPriorityOrder = c("State1", "State3", "State2"), # Priority order of states
#   stateCohortMandatory = c("State2"), # Mandatory states
#   stateCohortAbsorbing = c("State3"), # Absorbing states
#   ##############################################################################
#   # stateSelectionTypes
#   # 1 - First occurring
#   # 2 - Largest overlap
#   # 3 - Priority ordering
#   ##############################################################################
#   stateSelectionType = 3,
#   ##############################################################################
#   # trajectoryType
#   # 0 - Discrete time
#   # 1 - Continuous time
#   ##############################################################################
#   trajectoryType = 1,
#   lengthOfStay = 30,
#   outOfCohortAllowed = TRUE,
#   runSavedStudy = FALSE,
#   pathToResults = pathToResults,
#   useCDM = FALSE,
#   pathToData = paste(getwd(),'/tmp/datasets/importedData.csv', sep = ""),
#   allowedStatesList = allowedStatesList,
#  ##############################################################################
#  # oocFix (out of cohort fix)
#  # None - out of cohort state is included
#  # Last present state - out of cohort state is substituted with the latest state
#  # Some state name - out of cohort state is substituted with some state's notation
#  ##############################################################################
#  oocFix = "Last present state"
# )
