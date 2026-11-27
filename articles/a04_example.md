# A weekly worked example

This example uses synthetic intervals to build weekly trajectories with
four states: alive with or without recorded pneumonia history, and death
with or without that history. The analyst defines these states; the two
death states describe prior history, not cause of death. Each target
interval ends at death or the end of observation.

## Five patient histories

All patients enter on January 1, 2020. The examples cover:

- `001`: three weeks without pneumonia.
- `002`: pneumonia recorded on January 9, with history retained to
  observation end.
- `003`: pneumonia on January 10 and death on January 12, in the same
  weekly cycle.
- `004`: death without pneumonia history on January 4.
- `005`: observation ending on January 10, partway through the second
  cycle.

``` r

states <- c(
  "Alive, no recorded pneumonia",
  "Alive, pneumonia history",
  "Death, no pneumonia history",
  "Death, pneumonia history"
)

interval <- function(id, state, start, end) {
  data.frame(
    subject_id = id,
    cohort_definition_id = state,
    cohort_start_date = as.Date(start),
    cohort_end_date = as.Date(end)
  )
}

input <- do.call(rbind, list(
  interval("001", "0", "2020-01-01", "2020-01-21"),
  interval("001", states[1], "2020-01-01", "2020-01-21"),
  interval("002", "0", "2020-01-01", "2020-01-21"),
  interval("002", states[1], "2020-01-01", "2020-01-08"),
  interval("002", states[2], "2020-01-09", "2020-01-15"),
  interval("002", states[2], "2020-01-12", "2020-01-21"),
  interval("003", "0", "2020-01-01", "2020-01-12"),
  interval("003", states[1], "2020-01-01", "2020-01-09"),
  interval("003", states[2], "2020-01-10", "2020-01-11"),
  interval("003", states[4], "2020-01-12", "2020-01-12"),
  interval("004", "0", "2020-01-01", "2020-01-04"),
  interval("004", states[1], "2020-01-01", "2020-01-03"),
  interval("004", states[3], "2020-01-04", "2020-01-04"),
  interval("005", "0", "2020-01-01", "2020-01-10"),
  interval("005", states[1], "2020-01-01", "2020-01-10")
))
```

Import combines `002`’s overlapping history records without counting
shared days twice.

## Choose the weekly rules

Death takes priority over either living state. Pneumonia history takes
priority over the state without pneumonia. Both death states are
absorbing, so no further cycles are generated after a selected death
cycle.

``` r

studyPath <- tempfile("c2t-weekly-example-")
dir.create(studyPath)

allowed <- stats::setNames(list(
  states,
  states[c(2, 4)],
  character(0),
  character(0)
), states)

study <- cohort2TrajectoryConfiguration(
  studyName = "WeeklyExample",
  pathToStudy = studyPath,
  useCDM = FALSE,
  trajectoryDataObject = input,
  stateCohortLabels = states,
  stateCohortPriorityOrder = states[c(4, 3, 2, 1)],
  stateCohortAbsorbing = states[c(3, 4)],
  allowedStatesList = allowed,
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  cycleLength = 7
)
```

``` r

getDataForStudy(studyEnv = study)
trajectories <- createTrajectories(studyEnv = study)
```

## Inspect the weekly histories

``` r

clinical <- subset(trajectories, !state_label %in% c("START", "EXIT"))
knitr::kable(clinical[c(
  "subject_id", "state_label", "state_start_date", "state_end_date"
)], row.names = FALSE)
```

| subject_id | state_label                  | state_start_date | state_end_date |
|:-----------|:-----------------------------|:-----------------|:---------------|
| 001        | Alive, no recorded pneumonia | 2020-01-01       | 2020-01-07     |
| 001        | Alive, no recorded pneumonia | 2020-01-08       | 2020-01-14     |
| 001        | Alive, no recorded pneumonia | 2020-01-15       | 2020-01-21     |
| 002        | Alive, no recorded pneumonia | 2020-01-01       | 2020-01-07     |
| 002        | Alive, pneumonia history     | 2020-01-08       | 2020-01-14     |
| 002        | Alive, pneumonia history     | 2020-01-15       | 2020-01-21     |
| 003        | Alive, no recorded pneumonia | 2020-01-01       | 2020-01-07     |
| 003        | Death, pneumonia history     | 2020-01-08       | 2020-01-12     |
| 004        | Death, no pneumonia history  | 2020-01-01       | 2020-01-04     |
| 005        | Alive, no recorded pneumonia | 2020-01-01       | 2020-01-07     |
| 005        | Alive, no recorded pneumonia | 2020-01-08       | 2020-01-10     |

For `003`, cycle two is labelled `Death, pneumonia history`. It
preserves the history category but not the January 10 onset date as a
separate row.

For `004`, the first cycle is a death cycle even though death occurred
on January 4. Row dates describe the cycle. Both patients’ final cycles
end on their death dates because their target intervals end there. With
a later target endpoint, absorption would stop after the cycle without
shortening it to the event date.

For `005`, the second cycle covers January 8 to 10. Its January 11
`EXIT` marker adds no observed time and does not indicate death.

The completed run saves these rules in
`WeeklyExample/Settings/analysisSettings.rds`. See [Import cohort
data](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.md)
to rerun them with fresh data in a new location.

## Before fitting a model

The four labels can be four model states. For a model of onset followed
by mortality within a week, create a second view retaining pneumonia
history instead of selecting death. Keep identical patient-week
boundaries and observation endpoints in both views, with no weeks added
after death. TMA’s `prepareStageObservations()` pairs them and
`estimateStageTransitions()` fits the stages. See TMA’s
`inst/examples/c2t-to-icer.R` for an intervention example.

A requirement that pneumonia precede death by at least one day belongs
in the cohort definitions. Weekly labels cannot establish that
separation. Use the underlying intervals when exact dates matter; [Read
the
output](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
lists their locations.
