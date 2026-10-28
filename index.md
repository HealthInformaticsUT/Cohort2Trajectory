# Cohort2Trajectory

Cohort2Trajectory turns cohort intervals into patient trajectories. The
target cohort defines each patient’s observation period. State cohorts
describe what happened during that period. The analyst defines those
cohorts and decides which state takes priority when records overlap.

The package accepts cohorts generated in an OMOP Common Data Model
database, an R data frame or a CSV file. It produces either fixed-length
cycles or continuous intervals. It does not infer diagnoses, recovery or
a history state from the names of the supplied cohorts.

## Start here

The [documentation
site](https://healthinformaticsut.github.io/Cohort2Trajectory/) contains
the function reference and these articles:

- [Getting
  started](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a00_introduction.html):
  a small example that needs no database.
- [Configure a
  study](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.html):
  observation bounds, selection rules and overlapping states.
- [Import cohort
  data](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.html):
  local data, ATLAS definitions and saved studies.
- [Create
  trajectories](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.html):
  discrete and continuous output.
- [A weekly worked
  example](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a04_example.html):
  pneumonia history, two death states and a saved-study rerun.
- [Read the
  output](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.html):
  columns, dates, saved files and downstream use.

The usual workflow has three steps:

``` r

study <- cohort2TrajectoryConfiguration(
  studyName = "MyStudy",
  pathToStudy = tempdir(),
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

Here `input` is your cohort table. It needs `subject_id`,
`cohort_definition_id`, `cohort_start_date` and `cohort_end_date`. Use
`"0"` for target-cohort rows and the clinical labels for state rows.
Supply dates as `Date` values in R or `YYYY-MM-DD` in CSV. The
getting-started article supplies a complete input table.

Use names such as `"Discrete"` and `"Priority"` for the public options.
They ignore case and surrounding spaces. Numeric option codes are not
accepted.

## What the trajectory means

A discrete trajectory assigns one state to each cycle. A seven-day cycle
has seven days, including both endpoints. The final cycle can be
shorter. Its selected state does not mean that the patient spent every
day in that state. For example, a death recorded within the week can
determine the week’s label.

A continuous trajectory keeps selected interval dates. It does not fill
gaps or move simultaneous records onto different dates. In either
format, `START` and `EXIT` mark boundaries, not clinical states.
`OUT OF COHORT` means that no state was selected for a discrete cycle.
It does not mean observation ended.

Overlapping states can be combined with `mergeStates = TRUE`. Membership
is stored explicitly. A label containing `+` is never split to guess its
members. Use
[`mergedState()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/mergedState.md)
when a particular combination needs different allowed destinations,
absorbing status or a display label. The configuration article explains
the inheritance rules and their limits.

## Rerun a saved study

A successful import and trajectory run saves its rules in
`Settings/analysisSettings.rds`. For CDM studies, this file also
contains the cohort JSON definitions used during import.

Copy the completed study’s `Settings` folder to the new study folder. Do
not copy `Data` when sharing a study because it contains patient-level
records. For a CDM rerun, supply a connection to the new database:

``` r

study <- cohort2TrajectoryConfiguration(
  studyName = "MyStudy",
  pathToStudy = "/new/location",
  runSavedStudy = TRUE
)
getDataForStudy(cdm = newCdm, studyEnv = study)
trajectories <- createTrajectories(cdm = newCdm, studyEnv = study)
```

There is no need to re-enter the analytical settings or connect to
ATLAS. For local data, also set `useCDM = FALSE` and supply fresh input
through `trajectoryDataObject` or `pathToData`. To change the analysis
rules, configure a new run instead of overriding a saved study.

Only the current RDS format, version 4, is supported. Older studies must
be reconfigured and rerun. Settings CSV files and the removed arguments
`mergeThreshold` and `updateAutomaticallyForMergedStates` are not
supported.

## Database access

CDM studies use a CDMConnector connection. The database account needs
read access to the required CDM and vocabulary tables, and permission to
create and populate tables in the configured write schema. Start with a
small cohort when checking a new server connection.

## Related packages

- [CohortContrast](https://github.com/HealthInformaticsUT/CohortContrast)
- [TrajectoryMarkovAnalysis](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis)
- [TrajectoryViz](https://github.com/HealthInformaticsUT/TrajectoryViz)

These packages have separate interfaces. Do not assume that an older
analysis package can read the current trajectory output without checking
its expected column names and handling of boundary rows.

## Publication and citation

Haug M, Oja M, Pajusalu M, et al. *Markov modeling for
cost-effectiveness using federated health data network*. Journal of the
American Medical Informatics Association, 2024.
[doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).

When reporting an analysis, also record the package version or Git
commit and retain the settings used for the run.

## License and contact

Apache License 2.0. Developed by the University of Tartu Health
Informatics research group. Contact [Markus
Haug](mailto:markus.haug@ut.ee).
