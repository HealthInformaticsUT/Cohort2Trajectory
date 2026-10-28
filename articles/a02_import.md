# Import cohort data

## Import the configured study

[`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
retrieves the cohort records, cleans their intervals and saves the input
for trajectory generation. It needs the study environment returned by
[`cohort2TrajectoryConfiguration()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md).
See
[configuration](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.md)
if you have not created one.

## CDM data

For a study configured with `useCDM = TRUE`, pass the connected CDM
reference:

``` r

getDataForStudy(cdm = cdm, studyEnv = studyEnv)
```

With a non-NULL `baseUrl`, the package downloads the selected cohort
definitions from the ATLAS WebAPI. CDMConnector then generates those
cohorts on the supplied database and retrieves their records. ATLAS
supplies definitions, not patient data. `atlasTargetCohort` and
`atlasStateCohorts` refer to definition IDs, not rows in a previously
generated cohort table.

The target definition is stored as `JSON/0.json`. State definitions use
`JSON/atlas_<id>.json`. The package creates `CohortsToCreate.csv` to map
files to generated cohorts. Clinical labels are kept separate from these
filenames.

### Use local cohort JSON instead of ATLAS

Set `baseUrl = NULL` and supply filenames without `.json`. Paths are
relative to the study’s JSON folder. For this example, place
`target.json`, `states/A.json` and `states/B.json` under
`local-json/JSON` after configuration. They must contain the cohort
expressions used to generate the cohorts.

``` r

studyEnv <- cohort2TrajectoryConfiguration(
  studyName = "local-json",
  pathToStudy = getwd(),
  useCDM = TRUE,
  baseUrl = NULL,
  atlasTargetCohort = "target",
  atlasStateCohorts = c("states/A", "states/B"),
  stateCohortLabels = c("A", "B"),
  stateCohortPriorityOrder = c("B", "A"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  lengthOfStay = 7
)

# Place the JSON files in the study folder before this call.
getDataForStudy(cdm = cdm, studyEnv = studyEnv)
```

The package still generates cohorts on the CDM. Only the source of their
definitions has changed. Relative subfolders are supported. Do not use
absolute paths or parent traversal in definitions intended for saved
replay.

CDM imports work in a temporary JSON folder. Downloads and the new
manifest are published only after a successful import. Failed downloads,
queries and empty eligible cohorts leave the previous working
definitions unchanged. Unrelated JSON files are not replaced.

## Local data

Set `useCDM = FALSE` to import a data frame or CSV without a database.
Use these four column names exactly:

| Column | Content |
|----|----|
| `subject_id` | Patient identifier. Use character values for leading zeros or large IDs. |
| `cohort_definition_id` | Character label. `"0"` identifies a target interval. Other values are state labels. |
| `cohort_start_date` | Interval start, as an R `Date` in a data frame. |
| `cohort_end_date` | Interval end, as an R `Date` in a data frame. |

For CSV input, write dates as `YYYY-MM-DD`. Dates must be present and
the end must not precede the start. Intervals include both endpoints.
Patient IDs must not be missing or blank. Numeric whole-number and
`integer64` IDs are also accepted, but use character or `integer64`
before creating values at or above `2^53` to avoid rounding in R
doubles.

The target label is the text `"0"`, regardless of the clinical labels.
This example includes a second patient with a target interval and no
state records:

``` r

rawData <- data.frame(
  subject_id = c("001", "001", "001", "002"),
  cohort_definition_id = c("0", "A", "B", "0"),
  cohort_start_date = as.Date(c(
    "2020-01-01", "2020-01-01", "2020-01-08", "2020-01-01"
  )),
  cohort_end_date = as.Date(c(
    "2020-01-21", "2020-01-07", "2020-01-14", "2020-01-21"
  ))
)

studyEnv <- cohort2TrajectoryConfiguration(
  studyName = "local-data",
  pathToStudy = getwd(),
  useCDM = FALSE,
  trajectoryDataObject = rawData,
  stateCohortLabels = c("A", "B"),
  stateCohortPriorityOrder = c("B", "A"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  lengthOfStay = 7
)

getDataForStudy(studyEnv = studyEnv)
```

To configure a CSV source instead, omit `trajectoryDataObject` and
supply `pathToData = "path/to/input.csv"`. The package reads patient IDs
and state labels as text, preserving leading zeros and the literal label
`"NA"`.

### Replace the input for one import

An explicit local source takes precedence over the configured
alternative:

``` r

# Use this CSV even if the configuration contains a data frame.
getDataForStudy(studyEnv = studyEnv, pathToData = "path/to/new-input.csv")

# Use this data frame even if the configuration contains a CSV path.
getDataForStudy(studyEnv = studyEnv, trajectoryDataObject = newData)
```

Do not supply both non-NULL sources in one call. With neither supplied,
the configured data frame is used when present, otherwise the configured
CSV is used. Set `trajectoryDataObject = NULL` in an import call to use
the configured CSV instead. These overrides apply to that import only.
CDM imports ignore both local source arguments.

## What cleaning does

Only patients with a target row are retained. If a patient has several
target intervals, the first interval by start date supplies the bounds.
State records before its start are removed or clipped. By default,
records after its end are also removed or clipped.
`outOfCohortAllowed = TRUE` allows retained state records to extend
beyond the target end, without moving the target start.

Overlapping records for the same patient and state are combined. Every
label in `stateCohortMandatory` must occur within the retained interval.
This checks presence anywhere in that interval, not eligibility on Day
7. Use `NULL` or `character(0)` when no state is mandatory. Patients
with only a target interval remain eligible when no mandatory rule
excludes them.

When `mergeStates = TRUE`, overlaps between different states are then
represented by their structured membership. Import does not infer
diagnoses, prior history or clinical meaning from the state labels.

If no eligible patients remain, import does not succeed. Previous
cleaned files are kept, and trajectory generation is blocked until a
later import succeeds. Changing mandatory-state requirements also
requires another import.

## Saved studies

Restore a version 4 snapshot with `runSavedStudy = TRUE`, then import
fresh data. See [saved-study
configuration](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.html#repeat-a-saved-study)
for the Settings-only sharing workflow. A saved CDM study uses the
definitions embedded in the RDS file, without an ATLAS connection or
working JSON files:

``` r

getDataForStudy(cdm = newCdm, studyEnv = savedStudy)
```

For a saved local-data study, configure `useCDM = FALSE` and supply a
new local source. The same per-call source precedence applies. Saved
runs restore their analysis settings, so do not pass replacement labels,
mandatory states, transition rules or merging options.

After import, continue to [trajectory
generation](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.md).
The [output
article](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
describes cleaned data, trajectories and the saved settings file.
