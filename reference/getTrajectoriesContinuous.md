# Construct continuous patient histories

This function returns selected state intervals with trajectory boundary
markers.

## Usage

``` r
getTrajectoriesContinuous(
  cdm,
  patientData,
  stateSelection,
  statePriorityVector,
  absorbingStates = NULL,
  studyName = "",
  addPersonalData = TRUE,
  allowedStatesList = list(),
  stateLevels = NULL,
  outOfCohortAllowed = FALSE
)
```

## Arguments

- cdm:

  a cdm object

- patientData:

  Object of class data.frame with columns subject_id, state_label,
  STATE_START_DATE, STATE_END_DATE

- stateSelection:

  Selection of the type of ordering

- statePriorityVector:

  All states in prioritized order

- absorbingStates:

  Absorbing states in dataframe

- studyName:

  name chosen for study, is used as a folder name

- addPersonalData:

  Logical, indicating whether or not to add personal data for each
  patient

- allowedStatesList:

  A list object which indicates accessible states from said state

- stateLevels:

  Ordered study-wide state labels used to assign stable IDs.

- outOfCohortAllowed:

  Whether retained states may extend the target endpoint.

## Value

A dataframe of selected intervals and boundary markers. Models that
require unique timestamps need an explicit rule for simultaneous
records.

## Details

Target intervals supply START and EXIT boundaries even without state
records. Selected intervals retain their endpoints and simultaneous
records retain equal elapsed times. Gaps do not imply continued clinical
state membership. Absorbing states end the trajectory at their retained
endpoint. Earlier overlapping intervals are capped at that endpoint so
that no clinical interval extends beyond the shortened follow-up.
Priority selection assigns shared days to the higher-priority state and
retains only non-empty fragments of lower-priority intervals. Only
transition-eligible states compete for priority. Continuing a selected
interval does not require a self-transition when another record starts
or ends. A selected absorbing interval retains its bounded recorded
endpoint. Every clinical state in the bounded input must appear in
statePriorityVector. Missing states raise an error rather than being
silently discarded. Transition rules are checked before the
absorbing-state cutoff. Empty rules mean unrestricted transitions.
Rejected records do not change the last retained state, and only a
retained absorbing record can end the trajectory.
