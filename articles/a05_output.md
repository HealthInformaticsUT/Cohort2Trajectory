# Read the output

[`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md)
returns a `c2t_trajectories` data frame and writes its rows to
`Data/patientDataDiscrete.csv` or `Data/patientDataContinuous.csv` in
the study folder.

Printing shows patient and state counts, boundary and unresolved-row
counts, and a short preview. Every row and column remains available:

``` r

trajectories                  # Compact preview
print(trajectories, n = 10)    # Show more rows
as.data.frame(trajectories)   # Ordinary data-frame display
```

## Columns

| Column | Meaning |
|:---|:---|
| `subject_id` | The original patient identifier. Internal batching IDs are not exported. |
| `episode_id` | Present only with `targetEpisodeHandling = "All"`. Chronological target-period number within the patient, assigned before eligibility filtering. |
| `state_label` | The selected clinical label, or `START`, `EXIT` or `OUT OF COHORT`. |
| `state_start_date` | First date of the selected interval or discrete cycle. |
| `state_end_date` | Last date of that interval or cycle, including this date. |
| `time_in_cohort` | Years from the observation period’s start to this row’s start, calculated as elapsed days divided by 365.25. |
| `seq_ordinal` | Consecutive occurrence number of the same label in that patient’s output rows. It resets when the label changes. It is not a cycle number. |
| `state_id` | An identifier assigned to the label within the run. It is not a clinical concept ID or a cross-database key. |
| `age` | Age in years at this row’s start, when a CDM demographic join is used. Otherwise `NA`. |
| `gender_concept_id` | The CDM person’s recorded gender concept ID, when joined. Otherwise `NA`. |
| `state_key` | Present only for merged studies. Identifies the original members of a state across runs with the same labels. |

In `"All"` mode, `START`, `EXIT`, cycles, `time_in_cohort` and
`seq_ordinal` restart in each episode. Use both `subject_id` and
`episode_id` to identify a history. State IDs and the state dictionary
cover the whole study. No rows represent the gaps between episodes.

Dates are `Date` values in R and date strings in CSV. Use column names
rather than positions. Treat `state_id` as an identifier; its type can
be character or numeric. Inspect unmerged IDs with
`unique(trajectories[c("state_id", "state_label")])`.

When reading a CSV, preserve patient identifiers as text:

``` r

trajectories <- read.csv(
  "MyStudy/Data/patientDataDiscrete.csv",
  colClasses = "character",
  na.strings = NULL
)
trajectories$state_start_date <- as.Date(trajectories$state_start_date)
trajectories$state_end_date <- as.Date(trajectories$state_end_date)
for (column in c("time_in_cohort", "seq_ordinal", "age", "gender_concept_id")) {
  trajectories[[column]] <- type.convert(trajectories[[column]], as.is = TRUE)
}
```

This preserves leading zeros, long identifiers and literal labels such
as `"NA"`, then converts known numeric columns. Supply large in-memory
input IDs as character or `integer64`; digits rounded by numeric storage
cannot be recovered.

## Observation boundaries and unresolved cycles

By default, the first target interval supplies observation bounds. A
selected absorbing state can end follow-up early;
`outOfCohortAllowed = TRUE` lets retained records extend its end, but
not its start. With `targetEpisodeHandling = "All"`, every episode has
its own target bounds and `outOfCohortAllowed` must be `FALSE`. Later
episodes remain eligible after absorption unless
`stopAfterAbsorbingEpisode = TRUE`.

`START` marks the first date. `EXIT` is the day after the retained
follow-up endpoint: the final cycle end for discrete output. Each marker
has identical start and end dates and contributes no observed days.
Exclude them when estimating time spent in clinical states.

`OUT OF COHORT` is a discrete cycle with no selected state, within
observation. Deleting these rows can make separated states look
adjacent. Decide how to handle them before estimating transitions.

## Discrete dates are cycle dates

With `cycleLength = 7`, a cycle beginning January 1 ends January 7. The
next begins January 8. If observation ends January 10, that final cycle
contains three observed days. For non-marker rows, duration is
`as.integer(state_end_date - state_start_date) + 1`.

One label represents the whole cycle. Its dates need not match the
selected event’s dates. A selected absorbing state stops generation
after its cycle. Absorption does not shorten that cycle to the event
date; an observation boundary on that date does.

History-specific death states can preserve a prior complication
category, but not its exact date within the cycle. See the [weekly
example](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a04_example.md).

## Continuous dates are interval dates

Continuous output retains selected interval endpoints, subject to
observation and absorbing-state rules. Gaps remain unfilled and
simultaneous records keep their dates. Priority can split intervals;
First and Overlap can retain overlaps. Summing overlapping lengths
overcounts patient-time. Models requiring one state at every instant
need an explicit rule for these overlaps.

## State membership

For merged studies, `study$stateDictionary` and
`Data/stateDictionary.rds` describe each original or combined state:

| Field                | Meaning                                           |
|:---------------------|:--------------------------------------------------|
| `state_key`          | Stable key built from the original member labels. |
| `label`              | Display label used in the trajectory.             |
| `members`            | List of original state labels.                    |
| `absorbing`          | Whether selecting this state ends the trajectory. |
| `allowedTransitions` | List of permitted destination keys.               |
| `state_id`           | Run-local ID, added during generation.            |

Use `members` to inspect a combination. Do not split its display label
or key. The dictionary does not include boundary or unresolved-gap
markers. Unmerged studies do not produce this dictionary.

## Files to keep

| File | Purpose |
|:---|:---|
| `Settings/analysisSettings.rds` | Completed import and generation rules. For CDM studies it also contains the cohort JSON definitions used during import. |
| `Data/patientDataDiscrete.csv` or `Data/patientDataContinuous.csv` | Patient trajectories. |
| `Data/importedDataCleaned.csv` | Intervals used as input to trajectory generation. The filename is independent of the cohort-definition path. |
| `Data/sourceIntervals.rds` | Merged studies only. Cleaned intervals before overlapping states were combined. |
| `Data/stateDictionary.rds` | Merged studies only. Membership and resolved state rules. |

The interval files reflect observation limits, mandatory-state
requirements and combined overlapping records of the same state. In
merged studies, `sourceIntervals.rds` retains the original state labels
before composition.

Settings store `import` and `generation` separately. For example, after
`settings <- readRDS("MyStudy/Settings/analysisSettings.rds")`, the
cycle length is `settings$generation$cycleLength`. See [Import cohort
data](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.md)
for saved-study setup and format compatibility.

Share `Settings` to reproduce the rules. CDM settings include cohort
definitions; no settings file contains patient records or a database
connection. `Data` contains patient-level records.

## Handing the result to an analysis package

The `trajectory_definition` attribute records the format version,
effective trajectory type, cycle length, ordered clinical labels and
absorbing labels. Labels and absorbing status reflect merged-state
overrides; configured states remain declared when absent from the data.
Inspect it with `attr(trajectories, "trajectory_definition")`.

For default `"First"` output, TMA reads the version-1 definition
directly:

``` r

histories <- TrajectoryMarkovAnalysis::prepareTrajectories(trajectories)
transitions <- TrajectoryMarkovAnalysis::estimateTransitions(histories)
```

`"All"` output uses a version-2 definition to mark its separate
episodes. The current TMA reader accepts only version 1 and rejects this
output. An analysis must preserve episode boundaries rather than joining
the end of one episode to the start of the next. Removing the attribute
does not make such a join valid.

Use `saveRDS(trajectories, "trajectories.rds")` to preserve rows and
definitions. Ordinary data-frame row subsets also retain the attribute.
CSV preserves only rows. For `"First"` CSVs, supply cycle length, state
labels and absorbing states to TMA explicitly. TMA rejects settings
conflicting with an attached definition. Update the definition if a
transformation changes state meanings or cycle length.

Keep the separate state dictionary for merged membership. The handoff
contains labels and absorbing status, rather than the full selection and
transition rules.

Cohort2Trajectory constructs histories; downstream packages estimate
transitions, effects and costs. Check their treatment of boundaries,
gaps, overlaps and short final cycles. Exact event-time analyses need
interval dates rather than weekly cycle dates.
