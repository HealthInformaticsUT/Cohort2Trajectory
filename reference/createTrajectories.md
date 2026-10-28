# Generate patient trajectories from imported cohort intervals

Apply the configured selection, transition and gap rules to a successful
study import. The analyst defines clinical states and their intervals.
The target cohort supplies the observation bounds.

## Usage

``` r
createTrajectories(
  cdm = NULL,
  studyEnv = NULL,
  trajectoryType = studyEnv$trajectoryType,
  runSavedStudy = studyEnv$runSavedStudy,
  oocFix = studyEnv$oocFix,
  outOfCohortAllowed = studyEnv$outOfCohortAllowed,
  lengthOfStay = studyEnv$lengthOfStay,
  stateCohortLabels = studyEnv$stateCohortLabels,
  stateCohortPriorityOrder = studyEnv$stateCohortPriorityOrder,
  stateSelectionType = studyEnv$stateSelectionType,
  stateCohortAbsorbing = studyEnv$stateCohortAbsorbing,
  stateCohortMandatory = studyEnv$stateCohortMandatory,
  allowedStatesList = studyEnv$allowedStatesList,
  useCDM = studyEnv$useCDM,
  pathToStudy = studyEnv$pathToStudy,
  batchSize = studyEnv$batchSize,
  mergedStateOverrides = studyEnv$mergedStateOverrides
)
```

## Arguments

- cdm:

  A CDM reference created with CDMConnector, required when joining
  demographics with useCDM = TRUE.

- studyEnv:

  Study environment returned by cohort2TrajectoryConfiguration.

- trajectoryType:

  Trajectory format, either "Discrete" or "Continuous". Names are
  case-insensitive. Numeric choice codes are not accepted.

- runSavedStudy:

  Whether to use restored saved-study settings. Import fresh data first.
  Saved runs do not accept analysis-setting overrides.

- oocFix:

  Discrete gap rule. Use "None" or "OUT OF COHORT" to leave gaps
  unresolved, "Last present state" to carry an allowed state forward, or
  a state label as a fallback. See Details for label requirements.

- outOfCohortAllowed:

  Whether retained records may extend the target interval's end. The
  target start is unchanged.

- lengthOfStay:

  Positive whole-number cycle length in days for discrete trajectories.
  Use 7 for weekly cycles. Continuous output does not use it.

- stateCohortLabels:

  Nonblank, non-missing clinical state labels, unique ignoring case. The
  labels "0", "START", "EXIT" and "OUT OF COHORT" are reserved, also
  ignoring case.

- stateCohortPriorityOrder:

  State labels ordered from highest to lowest priority. Required for all
  discrete methods and continuous Priority.

- stateSelectionType:

  Selection method, either "First", "Overlap" or "Priority". Names are
  case-insensitive. A method must be specified.

- stateCohortAbsorbing:

  Labels of states that end a trajectory when retained. In nonmerged
  studies, use the exact spelling of a configured state, observed state
  or named discrete fallback. NULL and character(0) mean no absorbing
  states. Configured states may be absent from the data.

- stateCohortMandatory:

  Mandatory states used by the successful import. Rerun getDataForStudy
  to change them. Order and duplicates do not matter. NULL and
  character(0) both mean no requirements.

- allowedStatesList:

  Named list of allowed destinations for each source state. Use one
  unique, nonempty, non-missing source name per entry, such as list(A =
  c("B", "C")). An empty list imposes no restrictions.

- useCDM:

  Whether to join age and gender from the supplied CDM. Otherwise, age
  and gender_concept_id contain NA. No birth date is assumed or added.

- pathToStudy:

  Directory containing the study folder. Generation uses the location
  stored in studyEnv.

- batchSize:

  Positive whole-number maximum patients per batch.

- mergedStateOverrides:

  List of mergedState() definitions that replace selected inherited
  properties of merged states.

## Value

A data frame of trajectory rows, also saved as a CSV in the study's Data
directory. See the output vignette for the complete schema.

## Details

In discrete mode, First selects the earliest-starting eligible interval
in a cycle. Overlap selects the state covering the most distinct days in
that cycle, combining intervals without double-counting shared days.
Both methods use the priority order to resolve ties. Priority selects
the highest-ranked eligible state present during any part of the cycle.
Every clinical state in the bounded input must appear in the priority
order.

In continuous mode, First and Overlap both retain intervals that pass
the transition rules, including overlaps. Priority assigns shared days
to the highest-ranked eligible state and can split lower-priority
intervals. It also requires every clinical state to appear in the
priority order. Uninterrupted selected intervals do not require
self-transitions at unrelated record boundaries. Gaps are not filled and
simultaneous records keep their dates. Models requiring unique event
times need an explicit tie-handling rule.

Transition rules use the last retained clinical state. Rejected records
do not change that state. Source names must use the exact clinical
labels. Destination matching is case-insensitive. In a nonempty rule
list, each source used for a transition needs an entry. For merged
studies, a nonempty list must include every original state, including
absorbing states. character(0) permits no destinations. The first
clinical state and boundary markers need no incoming rule. Only an
allowed, retained absorbing state can end follow-up before the
observation boundary.

For discrete gaps, carry-forward requires an allowed self-transition and
a named fallback must be an allowed destination. Otherwise the cycle
remains OUT OF COHORT. This means no selected state, not loss of
observation, and does not reset the last retained state for later
transition checks. Supply one nonempty oocFix string. "None" and "Last
present state" always select gap-handling behavior, even if a clinical
state has that label. Boundary labels "0", "START" and "EXIT" are not
valid fallbacks. Use the exact spelling of an existing label. A new
label must not differ only in case from an existing clinical or
merged-state label. For merged states, use the display label rather than
state_key. Declare key-like text in stateCohortLabels if it is intended
as a separate literal state.

## Boundaries and timing

The first target interval, labelled 0 in cleaned data, supplies the
start and end boundaries. Patients without clinical state records remain
eligible unless mandatory-state requirements exclude them. Discrete
cycles include both endpoint dates, and the final cycle is shortened at
the observation boundary. A selected absorbing state stops generation
after its cycle. The cycle is not independently shortened to that
state's original onset date.

Continuous trajectories end at the retained absorbing interval's end
date. Earlier overlapping intervals are capped at that endpoint. START
and EXIT are boundary markers and do not add observed time. EXIT is on
the day after the last discrete cycle or continuous follow-up endpoint.
Its time_in_cohort is elapsed years from trajectory start, using 365.25
days per year.

Setting outOfCohortAllowed = TRUE can extend the endpoint using retained
state records, but cannot recover records removed during import. Public
imports require a target row for each included patient. The package does
not infer observation coverage, episode duration or clinical history. An
analyst-defined history-specific terminal state preserves its category
when selected, but a discrete row does not retain exact event dates or
ordering within its cycle.

## Study workflow

Run getDataForStudy successfully before generation. An unsuccessful
import blocks generation until a later import succeeds. Existing output
files are preserved but are not reused for that failed attempt. The
trajectory CSV is written only after all patient batches finish, so a
batch error does not replace a previous complete file with a partial
result.

A completed fresh run saves Settings/analysisSettings.rds in version 4
format. This preserves analysis settings and any imported CDM definition
contents, not patient data. NULL, character(0) and nested lists retain
their R types. Only version 4 snapshots are accepted. Saved runs restore
their analysis rules and require fresh input. Configure a new study to
change those rules.

## Labels and identifiers

State IDs are assigned once per study. In unmerged studies, START has ID
1, followed by configured states in their supplied order, additional
input and discrete gap labels in alphabetical order, then EXIT.
Configured states keep their IDs when absent from a batch. Merged
studies use dictionary order and add state_key to the output. Their
stateDictionary is available in studyEnv and Data/stateDictionary.rds.
Use keys and membership for cross-site comparison rather than run-local
state IDs. Original-state rules remain unchanged in the configuration.

Patient IDs are mapped internally and restored before demographic joins
and output. File and CDM input preserve large identifiers and leading
zeros as text. Canonical numeric IDs below 2^53 retain numeric output.
Supply larger in-memory IDs as character or integer64 because doubles
may already have rounded them. When reading exported CSVs, read
subject_id as character to preserve long or zero-prefixed identifiers.
Temporary IDs are not exported.

Working labels and rules are converted to UTF-8 without changing
spelling or case. Configuration and saved settings retain the supplied
labels.

## See also

[`vignette("a03_trajectories", package = "Cohort2Trajectory")`](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.md),
[`vignette("a05_output", package = "Cohort2Trajectory")`](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
getDataForStudy(cdm = cdm, studyEnv = studyEnv)
trajectories <- createTrajectories(cdm = cdm, studyEnv = studyEnv)
} # }
```
