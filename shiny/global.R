################################################################################
#
# Reading in saved data
#
################################################################################

jsonFiles <- list.files(
  path = paste(pathToResults, "/inst/JSON/", sep = ""),
  pattern = NULL,
  all.files = FALSE,
  full.names = TRUE
)
stateNamesJSON <- list.files(
  path = paste(pathToResults, "/inst/JSON/", sep = ""),
  pattern = NULL,
  all.files = FALSE,
  full.names = FALSE
)
stateNamesJSON <<- substr(stateNamesJSON, 1, nchar(stateNamesJSON) - 5)
targetIndex <- which(stateNamesJSON == "0")
stateNamesJSON <- stateNamesJSON[-targetIndex]
targetJSON <- if (identical(jsonFiles[targetIndex], character(0))) {
    "" } else {
  paste(readLines(jsonFiles[targetIndex]), collapse = "\n")
}
jsonFiles <-  jsonFiles[-targetIndex]

insertedJSONs <- c()

for (jsonFile in jsonFiles) {
  insertedJSONs <-
    c(insertedJSONs, paste(readLines(jsonFile), collapse = "\n"))
}

################################################################################
#
# Loading saved settings
#
################################################################################

settings <-
  read.csv(paste(pathToResults, "/inst/Settings/trajectorySettings.csv", sep = ""))
pathToResults <<- pathToResults
savedStudyName <<- settings$studyName
studyIndex <- which(studyName == savedStudyName)
studyHasBeenSaved <- if (length(studyIndex) == 0) {
  FALSE
} else {
  TRUE
}

savedTrajectoryType <- settings$trajectoryType[studyIndex]
savedTrajectoryStates <-
  strsplit(settings$trajectoryStates[studyIndex], ",")
savedPriorityOrder <-
  strsplit(settings$priorityOrder[studyIndex], ",")
savedStateSelectionType <- settings$stateSelectionType[studyIndex]
savedAbsorbingStates <- settings$absorbingStates[studyIndex]
savedMandatoryStates <- settings$mandatoryStates[studyIndex]
savedLengthOfStay <- settings$lengthOfStay[studyIndex]
savedOutOfCohortAllowed <- settings$outOfCohortAllowed[studyIndex]
savedOutOfCohortFix <- settings$outOfCohortFix[studyIndex]
