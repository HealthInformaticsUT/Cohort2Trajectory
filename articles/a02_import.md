# Import cohort data

## Import the configured study

[`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
retrieves the cohort records, cleans their intervals and saves the input
for trajectory generation. It needs the study environment returned by
[`cohort2TrajectoryConfiguration()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md)
and updates that environment after a successful import. It returns
`NULL` invisibly; keep the environment for the call to
[`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md).
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
definitions from the ATLAS WebAPI. CDMConnector generates those cohorts
on the supplied database and retrieves their records.
`atlasTargetCohort` and `atlasStateCohorts` identify these definitions.
Patient records come from the connected CDM database.

The target definition is stored as `JSON/0.json`. State definitions use
`JSON/atlas_<id>.json`. The package creates `CohortsToCreate.csv` to map
files to generated cohorts. Clinical labels are kept separate from these
filenames.

The database account must be able to create and drop tables in its write
schema. Generated cohorts use a unique package-owned name and are
removed after their records have been collected. Existing tables are
preserved, and the database connection stays open.

For an authenticated WebAPI, pass the required HTTP headers in the
import call:

``` r

getDataForStudy(
  cdm = cdm, studyEnv = studyEnv,
  webApiHeaders = c(Authorization = paste("Bearer", Sys.getenv("ATLAS_TOKEN")))
)
```

Headers are used only for the download and are excluded from saved
settings. No browser login is started. Use the header names and
authentication scheme required by your WebAPI, or use local cohort JSON.

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
  atlasStateCohorts = c(A = "states/A", B = "states/B"),
  stateCohortPriorityOrder = c("B", "A"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  cycleLength = 7
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

For CSV input, write dates as `YYYY-MM-DD`. Dates must be present, with
the end on or after the start. Intervals include both endpoints. Patient
IDs must be present and nonblank. Numeric whole-number and `integer64`
IDs are also accepted. Use character or `integer64` before creating
values at or above `2^53` to avoid rounding in R doubles.

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
  cycleLength = 7
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

Only patients with a target row are retained. With the default
`targetEpisodeHandling = "First"`, the first target interval by start
date supplies the bounds. State records are removed or clipped to those
bounds. `outOfCohortAllowed = TRUE` lets retained records extend the
target end, without moving its start.

With `targetEpisodeHandling = "All"`, identical target intervals are
deduplicated and each distinct interval becomes an episode. Overlapping
target intervals are rejected, including a shared endpoint because dates
are inclusive. Adjacent intervals are valid. States are clipped to every
intersecting episode; no rows are created for gaps between episodes.
This mode requires `outOfCohortAllowed = FALSE`. Set the period choice
during configuration or pass it to
[`getDataForStudy()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
for a fresh study; reimport to change it.

Overlapping records for the same patient and state are combined. Every
label in `stateCohortMandatory` must occur within each retained episode.
Eligibility is checked separately for every episode in `"All"` mode.
Episodes with no state records remain eligible when there are no
mandatory requirements. Episode numbers are assigned chronologically
within the patient before eligibility filtering, so retained episode IDs
can have gaps.

When `mergeStates = TRUE`, overlaps between different states are then
represented by their structured membership, as described in
[configuration](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.html#merge-overlapping-states-when-needed).

If no eligible patients remain, import does not succeed. Previous
cleaned files are kept, and trajectory generation is blocked until a
later import succeeds. Changing mandatory-state requirements also
requires another import.

## Saved studies

Restore a version 6, 5 or 4 snapshot with `runSavedStudy = TRUE`, then
import fresh data. See [saved-study
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
mandatory states, transition rules, episode handling or merging options.
Versions 4 and 5 restore `targetEpisodeHandling = "First"` and
`stopAfterAbsorbingEpisode = FALSE`.

After import, continue to [trajectory
generation](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.md).
The [output
article](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)
describes cleaned data, trajectories and the saved settings file.
