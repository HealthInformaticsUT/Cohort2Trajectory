# Getting started

Cohort2Trajectory builds patient histories from a target cohort and
state cohorts. The target cohort defines when each patient is observed.
The state cohorts supply the clinical intervals. You decide what those
intervals mean and how overlaps should be handled.

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

Choose seven-day cycles and put B ahead of A if they overlap. The input
here does not contain any overlap, but specifying the rule makes it
explicit.

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
  lengthOfStay = 7
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
`OUT OF COHORT` because no state was supplied. That label means an
unresolved cycle, not a loss of observation. The second cycle ends on
January 10, the target-cohort end date, rather than being extended to a
full week.

`START` is the start boundary. `EXIT` is the day after the last cycle
and does not add an observed day. Neither is a clinical state.

The function returns the trajectory table and saves it under
`Example/Data/patientDataDiscrete.csv`. The completed analysis settings
are saved under `Example/Settings/analysisSettings.rds`.

## Next steps

- [Configure a
  study](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.md)
  explains state selection, transitions and merging.
- [A weekly worked
  example](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a04_example.md)
  uses analyst-defined pneumonia-history and death states and checks a
  saved-study rerun.
- [Read the
  output](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
  explains the columns and what must be considered before downstream
  analysis.
