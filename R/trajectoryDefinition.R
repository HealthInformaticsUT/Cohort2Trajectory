# Describe the effective generation settings without retaining patient data or
# the study environment. stateLevels and absorbingStates use engine keys until
# the resolved registry maps them back to the labels returned to the analyst.
.trajectoryDefinition <- function(trajectoryType, cycleLength, stateLevels,
                                  absorbingStates, registry = NULL) {
  displayLabels <- function(states) {
    states <- as.character(states)
    if (!is.null(registry)) {
      matched <- match(states, registry$state_key)
      known <- !is.na(matched)
      states[known] <- registry$label[matched[known]]
    }
    unique(states[!states %in% c("START", "EXIT", "OUT OF COHORT", "0")])
  }
  list(
    version = 1L,
    trajectoryType = trajectoryType,
    cycleLength = if (trajectoryType == "Discrete") as.integer(cycleLength) else NULL,
    states = displayLabels(stateLevels),
    absorbingStates = displayLabels(absorbingStates)
  )
}
