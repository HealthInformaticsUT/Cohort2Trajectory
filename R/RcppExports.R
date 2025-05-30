# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

combineCohorts <- function(data, threshold, patientIDs) {
    .Call(`_Cohort2Trajectory_combineCohorts`, data, threshold, patientIDs)
}

getDiscreteStates <- function(stateSelection, oocFix, stateDuration, patientIDs, patientData, statePriorityVector, allowedStatesList) {
    .Call(`_Cohort2Trajectory_getDiscreteStates`, stateSelection, oocFix, stateDuration, patientIDs, patientData, statePriorityVector, allowedStatesList)
}

removeAfterAbsorbingStatesContinuous <- function(patientData, patientIDs, absorbingStates) {
    .Call(`_Cohort2Trajectory_removeAfterAbsorbingStatesContinuous`, patientData, patientIDs, absorbingStates)
}

removeAfterAbsorbingStatesDiscrete <- function(patientData, patientIDs, absorbingStates) {
    .Call(`_Cohort2Trajectory_removeAfterAbsorbingStatesDiscrete`, patientData, patientIDs, absorbingStates)
}

removeProhibitedTransitionsContinuous <- function(patientData, patientIDs, allowedStatesList) {
    .Call(`_Cohort2Trajectory_removeProhibitedTransitionsContinuous`, patientData, patientIDs, allowedStatesList)
}

