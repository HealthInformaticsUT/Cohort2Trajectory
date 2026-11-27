# Getting started

Cohort records describe intervals: when a patient was observed and when
each clinical state applied. Cohort2Trajectory turns those records into
patient histories using the same selection and transition rules for
every patient.

The target cohort defines each patient’s observation period. State
cohorts describe events or conditions within that period. You define
their clinical meaning, choose how to handle overlaps, and request
either fixed-length cycles or continuous intervals. The resulting table
can be used for trajectory plots or state-transition analysis.

This example uses local data, so you do not need a database connection.
For an OMOP database, see [Import cohort
data](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.md).

## Supply the cohort intervals

Patient `001` is observed for 14 days. State A covers the first week and
state B covers the second. Patient `002` is observed for ten days and
has no state records.

``` r

input <- data.frame(
  subject_id = c("001", "001", "001", "002"),
  cohort_definition_id = c("0", "A", "B", "0"),
  cohort_start_date = as.Date(c(
    "2020-01-01", "2020-01-01", "2020-01-08", "2020-01-01"
  )),
  cohort_end_date = as.Date(c(
    "2020-01-14", "2020-01-07", "2020-01-14", "2020-01-10"
  ))
)
```

`"0"` identifies target-cohort rows. Other values are state labels.
Dates include both endpoints. Keep identifiers as text if leading zeros
matter.

## Configure, import and generate

Choose seven-day cycles and put B ahead of A when they overlap. The
priority rule is part of the study settings, even though this example
has no overlaps.

``` r

studyPath <- tempfile("c2t-getting-started-")
dir.create(studyPath)

study <- cohort2TrajectoryConfiguration(
  studyName = "Example",
  pathToStudy = studyPath,
  useCDM = FALSE,
  trajectoryDataObject = input,
  stateCohortLabels = c("A", "B"),
  stateCohortPriorityOrder = c("B", "A"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  cycleLength = 7
)
getDataForStudy(studyEnv = study)
trajectories <- createTrajectories(studyEnv = study)
```

## Read the result

``` r

knitr::kable(trajectories[c(
  "subject_id", "state_label", "state_start_date", "state_end_date"
)], row.names = FALSE)
```

| subject_id | state_label   | state_start_date | state_end_date |
|:-----------|:--------------|:-----------------|:---------------|
| 001        | START         | 2020-01-01       | 2020-01-01     |
| 001        | A             | 2020-01-01       | 2020-01-07     |
| 001        | B             | 2020-01-08       | 2020-01-14     |
| 001        | EXIT          | 2020-01-15       | 2020-01-15     |
| 002        | START         | 2020-01-01       | 2020-01-01     |
| 002        | OUT OF COHORT | 2020-01-01       | 2020-01-07     |
| 002        | OUT OF COHORT | 2020-01-08       | 2020-01-10     |
| 002        | EXIT          | 2020-01-11       | 2020-01-11     |

Patient `002` is still included. Both cycles have the label
`OUT OF COHORT` because no state was supplied. This label marks a cycle
with no selected state. The second cycle ends on January 10, the
target-cohort end date.

`START` is the start boundary. `EXIT` is the day after the last cycle
and does not add an observed day. Neither is a clinical state.

The function returns the trajectory table and saves it under
`Example/Data/patientDataDiscrete.csv`. The completed analysis settings
are saved under `Example/Settings/analysisSettings.rds` so the study can
be repeated with fresh input. The returned table also carries its state
definitions and cycle length; use
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) to preserve these
when saving the table in R.

## Next steps

- [Configure a
  study](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.md)
  explains state selection, transitions and merging.
- [Create
  trajectories](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.md)
  includes a runnable example with multiple observation periods per
  patient.
- [A weekly worked
  example](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a04_example.md)
  uses analyst-defined pneumonia-history and death states.
- [Read the
  output](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
  explains the columns and what must be considered before downstream
  analysis.
