# Read the output

[`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md)
returns a data frame and writes the same trajectory to
`Data/patientDataDiscrete.csv` or `Data/patientDataContinuous.csv` in
the study folder. Use column names rather than their positions when
reading the result.

## Columns

| Column | Meaning |
|:---|:---|
| `subject_id` | The original patient identifier. Internal batching IDs are not exported. |
| `state_label` | The selected clinical label, or `START`, `EXIT` or `OUT OF COHORT`. |
| `state_start_date` | First date of the selected interval or discrete cycle. |
| `state_end_date` | Last date of that interval or cycle, including this date. |
| `time_in_cohort` | Years from trajectory start to this row’s start, calculated as elapsed days divided by 365.25. |
| `seq_ordinal` | Consecutive occurrence number of the same label in that patient’s output rows. It resets when the label changes. It is not a cycle number. |
| `state_id` | An identifier assigned to the label within the run. It is not a clinical concept ID or a cross-database key. |
| `age` | Age in years at this row’s start, when a CDM demographic join is used. Otherwise `NA`. |
| `gender_concept_id` | The CDM person’s recorded gender concept ID, when joined. Otherwise `NA`. |
| `state_key` | Present only for merged studies. Identifies the original members of a state across runs with the same labels. |

Dates are `Date` values in the returned R object and date strings in
CSV. `state_id` may be character or numeric depending on the trajectory
format. Treat it as an identifier, not as a quantity. For unmerged
output, inspect its mapping with
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

This keeps leading zeros, long identifiers and literal labels such as
`"NA"`. Only the known numeric columns are converted. For input already
held in R, use character or `integer64` values for large identifiers.
The package cannot recover digits already rounded by numeric storage.

## Observation boundaries and unresolved cycles

The first target-cohort interval supplies the trajectory’s start and
end. An absorbing state can stop the trajectory earlier. With
`outOfCohortAllowed = TRUE`, retained state records can extend its end,
but not move its start earlier. The analyst is responsible for supplying
valid observation bounds.

`START` marks the first date. `EXIT` marks the day after the retained
follow-up endpoint. For discrete output, that is the end of the final
cycle. Both markers have identical start and end dates. They do not
contribute observed days and should not be counted as clinical states
when estimating time spent in care.

`OUT OF COHORT` has a different meaning. It is a discrete cycle for
which no state was selected. The cycle is still within the supplied
observation period. Deleting such rows can make separated states look
like adjacent observations. Choose how an analysis handles these gaps
before estimating transitions.

## Discrete dates are cycle dates

With `lengthOfStay = 7`, a cycle beginning January 1 ends January 7. The
next begins January 8. If observation ends January 10, that final cycle
contains three observed days. For non-marker rows, duration is
`as.integer(state_end_date - state_start_date) + 1`.

One label represents the whole cycle according to the configured
selection rule. Its start date is not necessarily the date that the
selected event occurred. A selected death state stops generation after
its cycle. The cycle is shortened to the actual death date only if the
supplied trajectory boundary also ends there.

Separate death states can preserve whether a complication had occurred
before death, if the analyst defines them that way. They do not preserve
the exact within-week complication date. See the [weekly
example](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a04_example.md).

## Continuous dates are interval dates

Continuous output keeps the selected interval endpoints, bounded by the
target interval and absorbing-state rules. It does not invent state
coverage in a gap or shift simultaneous records onto different days.
Priority selection can split intervals. First and Overlap selection can
retain overlapping intervals.

Two rows can therefore share a start date or cover some of the same
days. A downstream model that needs one state at every instant must
handle that explicitly. Summing overlapping interval lengths will
overcount patient-time.

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
| `Data/importedDataCleaned_*.csv` | Intervals used as input to trajectory generation. The filename suffix identifies the target definition. |
| `Data/sourceIntervals.rds` | Merged studies only. Cleaned intervals before overlapping states were combined. |
| `Data/stateDictionary.rds` | Merged studies only. Membership and resolved state rules. |

The interval files have already been cleaned. Observation limits and
mandatory state requirements have been applied, and overlapping records
of the same state have been combined. They are not a copy of every
original database row. In merged studies, use `sourceIntervals.rds` to
inspect the original state labels before composition.

The version-4 settings file stores `import` and `generation` separately
because some choices can be supplied at generation time. For example,
the cycle length is under `settings$generation$lengthOfStay`. Clinical
rules remain the analyst’s responsibility even though their execution
settings are saved.

When sharing a study, copy `Settings`, not `Data`. The settings file
contains no patient records or database connection. It does include
study-specific cohort definitions, which should be reviewed before
sharing.

## Handing the result to an analysis package

Pass the trajectory together with the settings and, for merged studies,
the state dictionary. An analysis needs to know the cycle length,
boundary-marker meaning, absorbing states and state membership. Do not
infer these from label spelling or from the order of numeric IDs.

Cohort2Trajectory constructs histories. It does not estimate transition
probabilities, survival effects, costs or cost-effectiveness. Check that
the downstream package accepts the current column names and identifiers,
treats observation end separately from a clinical outcome, and handles
shortened cycles as intended. Exact event-time analyses also need the
relevant state intervals rather than dates inferred from weekly labels.
