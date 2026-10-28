# Convert the imported cohort data to patient treatmnet trajectories

This function outputs a data.frame object which describes the movement
of patients between the defined states

## Usage

``` r
getTrajectoriesDiscrete(
  cdm,
  cohortData,
  stateDuration = 30,
  stateSelection = 1,
  statePriorityVector = NULL,
  absorbingStates = NULL,
  oocFix = "None",
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

- cohortData:

  A data.frame object which is queried with getCohortsPatients function

- stateDuration:

  Positive whole-number cycle length in days. The final cycle is
  shortened when the trajectory boundary falls before a full cycle is
  complete.

- stateSelection:

  The chosen state selection method

- statePriorityVector:

  The order of priority for states

- absorbingStates:

  The absorbing states: patient trajectory ends with it

- oocFix:

  The method to use for replacing "OUT OF COHORT" states with more
  relevant states

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

## Details

EXIT is a boundary marker on the day after the final retained cycle. Its
time_in_cohort is elapsed time in years (days divided by 365.25) from
the trajectory start to that boundary. EXIT does not add observed time.
For all selection methods, every clinical state in the bounded input
must appear in statePriorityVector. First and Overlap use it to resolve
ties. Missing states raise an error, not an OUT OF COHORT replacement.
Overlap compares distinct covered days per state within each cycle,
combining separate intervals without double-counting repeated records.
Generation stops after the selected absorbing cycle, without checking
outgoing transition rules for later cycles. A prohibited absorbing
record does not end follow-up.
