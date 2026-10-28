# Configure a trajectory study

Choose the observation cohort, state definitions and rules for a study.
The analyst defines what each cohort means. This function creates the
study folders and returns an environment used by
[`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
and
[`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md).
It does not import data or generate trajectories.

## Usage

``` r
cohort2TrajectoryConfiguration(
  cdm = NULL,
  studyName = "Cohort2Trajectory",
  baseUrl = "http://localhost:8080/WebAPI",
  pathToStudy = getwd(),
  atlasTargetCohort = NULL,
  atlasStateCohorts = NULL,
  stateCohortLabels = NULL,
  stateCohortPriorityOrder = NULL,
  stateCohortMandatory = NULL,
  stateCohortAbsorbing = NULL,
  stateSelectionType = NULL,
  oocFix = "None",
  trajectoryType = "Discrete",
  lengthOfStay = NULL,
  outOfCohortAllowed = FALSE,
  runSavedStudy = FALSE,
  useCDM = TRUE,
  trajectoryDataObject = NULL,
  pathToData = "./Cohort2Trajectory/Data/importedData.csv",
  allowedStatesList = createStateList(stateCohortLabels),
  mergeStates = FALSE,
  batchSize = 1000,
  mergedStateOverrides = list()
)
```

## Arguments

- cdm:

  Not used by this function. Pass a connected CDMConnector object to
  [`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
  and
  [`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md)
  when those steps use a CDM.

- studyName:

  Study name, used as the folder name.

- baseUrl:

  ATLAS WebAPI URL for cohort definitions. Use `NULL` to read local
  definitions from the study's `JSON` folder during import.

- pathToStudy:

  Parent folder containing the study folder.

- atlasTargetCohort:

  ATLAS ID of the target cohort. With `baseUrl = NULL`, supply its local
  JSON filename without the extension. The first target interval defines
  each patient's observation period.

- atlasStateCohorts:

  ATLAS IDs of the state cohorts, in the same order as
  `stateCohortLabels`. With `baseUrl = NULL`, supply local JSON
  filenames without their extensions.

- stateCohortLabels:

  Character vector of state labels. Supply one per state cohort for CDM
  input. Labels must be nonempty, non-missing and unique even when
  ignoring case. `"0"`, `"START"`, `"EXIT"` and `"OUT OF COHORT"` are
  reserved, including case variants. Labels may contain spaces or
  punctuation and need not start with a letter. They are separate from
  database cohort names and definition filenames.

- stateCohortPriorityOrder:

  State labels from highest to lowest priority. All states within the
  observation bounds must be listed for discrete selection or continuous
  `"Priority"` selection. For a merged study, `NULL` uses a
  deterministic order of original states.

- stateCohortMandatory:

  State labels that every included patient must have within the retained
  observation bounds. `NULL` or `character(0)` imposes no requirement.
  All labels are literal, including `"No mandatory state"`. These
  requirements are applied during import. Rerun
  [`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
  to change them.

- stateCohortAbsorbing:

  State labels that end follow-up when selected. `NULL` or
  `character(0)` means none. For an unmerged study, use exact configured
  or observed labels, or a named discrete fallback. Configured states
  may be absent from the current data. For merged studies, supply
  original state labels here and use `mergedStateOverrides` for
  exceptions.

- stateSelectionType:

  `"First"`, `"Overlap"` or `"Priority"`. Names ignore case and
  surrounding whitespace. `NULL` is allowed during configuration, but
  generation needs a choice.

- oocFix:

  Discrete gap rule. `"None"` or `"OUT OF COHORT"` leaves an unresolved
  cycle. `"Last present state"` carries the last selected state forward
  when permitted. Any other nonempty, non-missing string names a
  fallback state. See the gap rules below.

- trajectoryType:

  `"Discrete"` or `"Continuous"`. Names ignore case and surrounding
  whitespace. `NULL` is allowed during configuration, but generation
  needs a choice.

- lengthOfStay:

  Positive whole-number cycle length in days, within R's integer range.
  Required when generating discrete trajectories. It may be `NULL`
  during configuration. The final cycle is shortened at the observation
  boundary.

- outOfCohortAllowed:

  Whether retained state records may extend the trajectory beyond the
  target interval's end. The target start is unchanged.

- runSavedStudy:

  Set `TRUE` to restore the completed study from
  `studyName/Settings/analysisSettings.rds`. Supply its location and
  fresh input or connections, without repeating analytical settings.

- useCDM:

  Whether to use a CDM for import and demographic joins during
  generation. Use `FALSE` for a local data frame or CSV.

- trajectoryDataObject:

  Data frame for local input, with columns `subject_id`,
  `cohort_definition_id`, `cohort_start_date` and `cohort_end_date`. Use
  label `"0"` for observation intervals and the chosen state labels for
  state intervals.

- pathToData:

  CSV path for local input when no data frame is configured. The CSV
  uses the same columns as `trajectoryDataObject`. Individual imports
  can select a different input through
  [`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md).

- allowedStatesList:

  Named list of allowed destinations, for example
  `list(A = c("B", "C"))`. Source names must be unique, nonempty and
  non-missing. Put all destinations for one source in a single entry. An
  empty list allows all transitions. `character(0)` or `NULL` in an
  individual entry permits no destinations.

- mergeStates:

  Set `TRUE` to represent overlapping state intervals as a combined
  state with explicit membership.

- batchSize:

  Maximum patients per generation batch. Supply one positive whole
  number within R's integer range. The default is 1000.

- mergedStateOverrides:

  List of
  [`mergedState()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/mergedState.md)
  definitions. Requires `mergeStates = TRUE`. Supply at most one
  definition for each membership set. Omitted properties inherit from
  the original states.

## Value

An environment containing the study settings.

## Observation and state definitions

The target cohort supplies observation intervals. State cohorts describe
events or periods within them, according to definitions chosen by the
analyst. The first target interval for each patient sets the trajectory
bounds, including for patients without state records.

Date intervals include both endpoints. A shared boundary date counts as
one day of overlap. Discrete trajectories assign a state to each cycle.
Continuous trajectories retain selected intervals and boundary markers
without filling gaps or changing the times of simultaneous events.

## State selection and gaps

In discrete trajectories, only overlapping states allowed from the last
selected state compete. `"First"` chooses the earliest start.
`"Overlap"` chooses the state covering the most distinct days in the
cycle, combining repeated records without counting a day twice.
`"Priority"` follows the supplied order. Ties use that order. Missing
priority entries raise an error for all three discrete methods.

If no state qualifies, `oocFix` applies. Carry-forward needs an allowed
self-transition, and a named fallback must be an allowed destination.
Otherwise the cycle is `"OUT OF COHORT"`. This label means no state was
selected, not that observation ended. It preserves the last selected
state for checking later transitions.

`"None"` and `"Last present state"` always select gap behaviour, even if
a clinical state has that label. Fallbacks cannot be `"0"`, `"START"` or
`"EXIT"`. Use the exact spelling of an existing state. A new fallback
must not differ only in case from an existing state. For a merged state,
use its display label, not its internal `state_key`. Text resembling a
key can be a separate literal state if declared in `stateCohortLabels`.

Continuous `"Priority"` selection resolves overlaps among allowed states
at each interval boundary. Every state within the bounds must appear in
the priority order. An ongoing interval does not need a self-transition
just because another record starts or ends. A selected absorbing
interval keeps its bounded recorded endpoint even if a higher-priority
record arrives later. Models requiring unique timestamps must resolve
simultaneous events.

## Transitions and absorbing states

Source names use exact state labels. Destination matching ignores case
using R's locale-aware conversion. Output preserves the clinical labels.
In an unmerged study with a nonempty rule list, an entry is required
only when a state is used as a transition source. Rules apply separately
to each patient and use the last retained clinical state.

A prohibited absorbing record does not end follow-up. Discrete
generation ends after the selected absorbing cycle. Continuous
generation ends at the retained absorbing interval's endpoint.
Generation does not check outgoing transitions after a retained
absorbing state.

## Combining overlapping states

With `mergeStates = TRUE`, exact overlaps form combined states before
discretisation. Membership is stored explicitly. Labels are never split
to infer membership. A nonempty original-state rule list must include
every original state. Use an empty list for unrestricted transitions.

Allowed destinations are inherited from the union of the source members'
destinations. Each member newly added at the destination must be allowed
by at least one source member. For a self-transition or a transition
that only removes members, every retained destination member needs that
permission.

A combination is absorbing if any member is absorbing. Its
highest-priority member determines its priority. At equal priority,
larger combinations come first, followed by state key order. Without a
supplied priority order, original states are ordered deterministically.
Explicit
[`mergedState()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/mergedState.md)
properties replace the inherited properties. Absorbing status takes
precedence over outgoing transitions.

The study environment's `stateDictionary` and the generated
`Data/stateDictionary.rds` record the resolved states and their rules.
See
[`mergedState()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/mergedState.md)
for individual exceptions.

## Saving and sharing a study

A completed import and generation saves the analysis settings in
`Settings/analysisSettings.rds`. Only version 4 is accepted. Reconfigure
and rerun older studies to create a current snapshot. CDM snapshots
contain the exact cohort JSON definitions captured at import. Saved runs
use those definitions and fresh patient data, leaving working JSON files
unchanged.

Copy the completed study's `Settings` folder to the new location. Do not
share `Data`, which contains patient records. Supply the new connection
to import and generation, or configure fresh local input with
`useCDM = FALSE`. Import must succeed before generation. To change
definitions or analysis rules, configure a fresh study. Conflicting
saved mandatory-state requirements also require a fresh run.

Local definition names may contain relative subpaths, such as `states/A`
for `JSON/states/A.json`. Saved paths cannot be absolute or contain
parent traversal.

## Patient identifiers

IDs may be numeric whole numbers, character or integer64 values. Missing
or blank identifiers are rejected. Temporary internal IDs are replaced
by the original identifiers before output. For in-memory IDs at or above
`2^53`, use character or integer64 input to avoid rounded doubles.

## See also

[`getDataForStudy`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md),
[`createTrajectories`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md),
[`mergedState`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/mergedState.md)

## Examples

``` r
if (FALSE) { # \dontrun{
study <- cohort2TrajectoryConfiguration(
  studyName = "ExampleStudy",
  pathToStudy = getwd(),
  useCDM = FALSE,
  trajectoryDataObject = cohorts,
  stateCohortLabels = c("A", "B"),
  stateCohortPriorityOrder = c("A", "B"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  lengthOfStay = 7
)
} # }
```
