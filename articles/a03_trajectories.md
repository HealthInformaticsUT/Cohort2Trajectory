# Create trajectories

## Before generation

The analyst defines the target cohort and the clinical state cohorts.
The target interval supplies the observation bounds. State intervals
determine when a patient belongs to each clinical state, including any
history states. Cohort2Trajectory applies the configured selection and
transition rules to these intervals. It does not infer clinical history,
episode duration or observation coverage.

Complete the
[configuration](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.md)
and [data
import](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.md)
before calling
[`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md).
Use the same `studyEnv` throughout the workflow.

``` r

getDataForStudy(cdm = cdm, studyEnv = studyEnv)
trajectories <- createTrajectories(cdm = cdm, studyEnv = studyEnv)
```

For local data, omit `cdm` and configure `useCDM = FALSE`. A failed
import blocks generation until another import succeeds. Previously saved
patient files are not used as a substitute for that failed import.

## Discrete trajectories

Discrete trajectories assign one state to each fixed-length cycle. Set
`lengthOfStay` to the number of calendar days per cycle. Despite its
name, this argument is a cycle length, not a hospital length of stay.
For example, use seven days for weekly trajectories:

``` r

trajectories <- createTrajectories(
  cdm = cdm,
  studyEnv = studyEnv,
  trajectoryType = "Discrete",
  stateSelectionType = "Priority",
  lengthOfStay = 7
)
```

Cycles begin at the target start date. Both ends of a cycle are
included, and the last cycle is shortened when the observation boundary
falls before a full cycle ends. The example uses the priority order
already configured in `studyEnv`.

The three selection methods choose among states that overlap the cycle
and are allowed by the transition rules:

| Method | Selection within a cycle |
|----|----|
| `"First"` | Choose the interval with the earliest start date, including intervals that began before the cycle. Resolve equal start dates using the priority order. |
| `"Overlap"` | Choose the state covering the most distinct days in the cycle. Combine separate intervals for the same state without counting overlapping days twice. Resolve equal coverage using the priority order. |
| `"Priority"` | Choose the highest-ranked state present during any part of the cycle. |

List every clinical state in `stateCohortPriorityOrder`, with the
highest priority first. This is required for all three discrete methods.
Priority is a ranking, not a way to exclude states from the analysis.

With `mergeStates = TRUE`, overlapping source intervals are composed
into states with structured membership during import. Selection then
uses those resolved states and their inherited rules, including any
explicit merged-state overrides. See
[Configuration](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.md)
for the merging rules.

Use the names `"Discrete"`, `"Continuous"`, `"First"`, `"Overlap"` and
`"Priority"` for these settings. Names are case-insensitive. Numeric
choice codes are not accepted by the public functions.

## Continuous trajectories

Continuous trajectories use calendar intervals rather than fixed cycles.
`lengthOfStay` does not determine their timing.

``` r

trajectories <- createTrajectories(
  cdm = cdm,
  studyEnv = studyEnv,
  trajectoryType = "Continuous",
  stateSelectionType = "Priority"
)
```

In continuous mode, `"First"` and `"Overlap"` have the same behavior.
They retain the bounded input intervals that pass the transition rules,
including overlapping intervals. They do not perform the cycle-level
comparisons described above.

Continuous `"Priority"` assigns shared days to the highest-priority
state that the transition rules permit. Lower-priority intervals can be
shortened or split into separate fragments. Every clinical state must
appear in the priority order. Continuing the same selected interval does
not require a new self-transition each time an unrelated record starts
or ends.

Continuous output does not extend a state across an undocumented gap.
Simultaneous records keep their dates and equal elapsed times. Their
ordering in the table is not evidence that one event occurred earlier
within that day. A downstream model that requires distinct event times
needs an explicit tie-handling rule.

## Transition rules and gaps

An empty `allowedStatesList` allows unrestricted transitions. Otherwise,
each state used as a transition source needs one named entry containing
all its allowed destinations. Use `character(0)` for a source with no
allowed destinations. Source names must match the state labels exactly.
Destination matching is case-insensitive. The first clinical state and
the `START` and `EXIT` boundary markers do not need an incoming
transition rule. For merged studies, a nonempty list must include every
original state, including absorbing states. Use `character(0)` for an
original state with no permitted destinations.

Rules are applied separately for each patient, using the last retained
clinical state. A rejected record does not become the source for later
transitions. In discrete mode, only allowed states compete for the
cycle. In continuous mode, prohibited records are excluded before the
absorbing-state cutoff.

When no state can be selected for a discrete cycle, `oocFix` controls
the result:

- `"None"` or `"OUT OF COHORT"` leaves the cycle as `OUT OF COHORT`.
- `"Last present state"` carries the last selected state forward only
  when its self-transition is allowed. Before the first selected state,
  the gap remains `OUT OF COHORT`.
- A state label supplies a named fallback, provided the transition to it
  is allowed. Use its exact spelling. For merged states, use the display
  label, not the internal `state_key`.

`OUT OF COHORT` means that no clinical state was selected for the cycle.
It does not mean that the patient was unobserved. An unresolved cycle
does not erase the last selected state when checking later transitions.
The strings `"None"` and `"Last present state"` always select their
gap-handling behavior, even if a clinical state has the same label. Gap
handling applies only to discrete output.

## Observation boundaries and absorbing states

The first target-cohort interval supplies each patient’s start and end
dates. Patients with no clinical state records can still have
trajectories if they meet the mandatory-state requirements. Their
discrete cycles follow the gap rule. Their continuous trajectories
contain the boundary markers without invented clinical intervals.

By default, state intervals are bounded by the target interval. With
`outOfCohortAllowed = TRUE`, retained state records can extend its end,
but not its start. Set this during import if those later records are
needed. Changing it only at generation cannot recover records removed
during import. Local input must include a target row for every patient
to be retained during import.

Only a selected, allowed absorbing state can end the trajectory before
its observation boundary. A prohibited or unselected absorbing record
does not end follow-up. No further transition checks are made after a
retained absorbing state.

For discrete trajectories, generation stops after the selected absorbing
cycle. The row still uses the cycle’s dates, so it is not automatically
shortened to the original event date. If the target interval itself ends
on that date, the final cycle is shortened at that boundary. For
continuous trajectories, follow-up ends at the retained absorbing
interval’s end date. Earlier overlapping intervals are also capped at
that endpoint.

`START` marks the trajectory start. `EXIT` marks the day after the last
retained cycle or the continuous follow-up endpoint. These markers do
not add observed time and do not, by themselves, indicate death.

## Clinical history and event timing

History belongs in the analyst’s state definitions. For example,
separate absorbing states for death with and without a prior pneumonia
history can retain that distinction in the selected terminal state.
Cohort2Trajectory does not construct that history classification from
the state labels.

A selected history-specific death state preserves the category, but it
does not recover exact event timing from a discrete cycle. If pneumonia
and death occur within one weekly cycle, the output may contain only the
selected death state. Its cycle dates do not reveal the pneumonia onset
date, the interval between the events or whether they occurred on the
same day. Changing the selection method does not make the weekly table a
complete event log.

Continuous trajectories and the cleaned interval files provide different
timing detail. Selection, target-boundary clipping and state merging can
still affect those intervals. See [Trajectory
output](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
for the output fields, date conventions and available files.

## Saved studies and output

For a fresh study, generation settings can be supplied directly as in
the examples above. Mandatory-state requirements are an import decision.
To change them, rerun
[`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
with the intended requirements before generating trajectories.

A completed fresh run saves `Settings/analysisSettings.rds` in the
current version 4 format. It contains the analysis settings and, for CDM
studies, the cohort definition contents used by the successful import.
It does not contain patient data. Only version 4 snapshots are accepted.
Older snapshots must be recreated by completing import and generation
with the current package.

After configuring a study with `runSavedStudy = TRUE`, import fresh data
and use the restored analysis settings without overrides:

``` r

getDataForStudy(cdm = newCdm, studyEnv = studyEnv)
trajectories <- createTrajectories(cdm = newCdm, studyEnv = studyEnv)
```

Saved CDM runs use the definitions embedded in the snapshot, not the
current working JSON files. Saved local runs use fresh local input.
Configure a new study if the analysis rules need to change.

[`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md)
returns a data frame and saves it in the study’s `Data` directory. The
trajectory CSV is written after all patient batches finish, so a batch
error does not replace it with a partial result.

With `useCDM = TRUE` and a supplied CDM object, generation joins age and
gender from the CDM. This can also be requested after importing local
cohort data. With `useCDM = FALSE`, `age` and `gender_concept_id` are
present but contain `NA`. No birth date is assumed. The [output
article](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
describes the full schema and how to interpret these columns.
