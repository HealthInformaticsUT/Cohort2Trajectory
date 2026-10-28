# Import and clean cohort intervals for a study

Retrieve cohort data from an OMOP CDM or read local cohort intervals,
then apply observation bounds, mandatory-state requirements and optional
state merging. The analyst defines the target and clinical state
cohorts.

## Usage

``` r
getDataForStudy(
  runSavedStudy = studyEnv$runSavedStudy,
  useCDM = studyEnv$useCDM,
  trajectoryDataObject = studyEnv$trajectoryDataObject,
  cdm = NULL,
  studyEnv = NULL,
  atlasStateCohorts = studyEnv$atlasStateCohorts,
  atlasTargetCohort = studyEnv$atlasTargetCohort,
  stateCohortLabels = studyEnv$stateCohortLabels,
  baseUrl = studyEnv$baseUrl,
  outOfCohortAllowed = studyEnv$outOfCohortAllowed,
  stateCohortMandatory = studyEnv$stateCohortMandatory,
  stateCohortPriorityOrder = studyEnv$stateCohortPriorityOrder,
  mergeStates = studyEnv$mergeStates,
  allowedStatesList = studyEnv$allowedStatesList,
  stateCohortAbsorbing = studyEnv$stateCohortAbsorbing,
  pathToData = studyEnv$pathToData,
  mergedStateOverrides = studyEnv$mergedStateOverrides
)
```

## Arguments

- runSavedStudy:

  Whether to import fresh data using the analysis rules restored by a
  saved-study configuration.

- useCDM:

  Whether to retrieve cohorts from the CDM. Use FALSE for a local data
  frame or CSV file.

- trajectoryDataObject:

  Data frame for a local import. An explicitly supplied non-NULL object
  overrides the configured CSV path. Supply NULL to use the CSV path.
  Overrides apply to this import only.

- cdm:

  A CDM reference created with CDMConnector.

- studyEnv:

  Study environment returned by cohort2TrajectoryConfiguration.

- atlasStateCohorts:

  ATLAS state cohort IDs, or local definition names without the .json
  extension when baseUrl is NULL.

- atlasTargetCohort:

  ATLAS target cohort ID, or local definition name without the .json
  extension when baseUrl is NULL.

- stateCohortLabels:

  Nonblank, non-missing clinical state labels, unique ignoring case. The
  labels "0", "START", "EXIT" and "OUT OF COHORT" are reserved, also
  ignoring case.

- baseUrl:

  OHDSI WebAPI base URL for downloading ATLAS definitions. Use NULL to
  read local JSON definitions.

- outOfCohortAllowed:

  Whether to retain state records beyond the target interval's end. The
  target start remains the lower boundary.

- stateCohortMandatory:

  States that every included patient must have within the retained
  observation bounds. NULL or character(0) imposes no requirements.
  Rerun import to change these requirements before generation.

- stateCohortPriorityOrder:

  State labels ordered from highest to lowest priority, also used to
  resolve inherited priorities for merged states.

- mergeStates:

  Whether to compose overlapping state intervals into states with
  structured membership.

- allowedStatesList:

  Named list of allowed destinations by source state. Each source name
  must be unique, nonempty and non-missing.

- stateCohortAbsorbing:

  Labels of absorbing states, also used to derive absorbing status for
  merged states.

- pathToData:

  CSV path for a local import. An explicitly supplied non-NULL path
  overrides the configured data object. Do not explicitly supply both
  non-NULL local sources.

- mergedStateOverrides:

  List of mergedState() definitions that replace selected inherited
  properties of merged states.

## Value

Invisibly returns NULL after a successful import. Cleaned data are saved
in the study's Data directory and studyEnv is updated for generation.

## Details

The target cohort defines observation bounds. Clinical state intervals,
including history states and finite episodes, must be defined by the
analyst before import. State labels are preserved for CDM, data-frame
and CSV input. Spaces and punctuation are not replaced to make
filenames. Use the same labels in mandatory states, priorities and
transition rules. CSV cohort_definition_id values are read as text,
preserving leading zeros and the literal label "NA". Empty labels are
invalid. All mandatory-state labels are literal, including "No mandatory
state".

A nonempty target cohort remains valid when no clinical state records
survive cleaning, provided mandatory-state requirements do not exclude
its patients. The target intervals still define follow-up. Discrete
trajectories use the configured gap rule. Continuous trajectories retain
boundaries without inventing clinical states.

An empty eligible cohort is not a successful import. Previous cleaned
data, merged-state artifacts and saved settings remain unchanged. An
unsuccessful import blocks trajectory generation until a later import
succeeds.

## Local input

With neither local source explicitly supplied, the configured data
object is used when present, otherwise the configured CSV path is used.
An explicit non-NULL source overrides the opposite configured source for
this call only. Supplying trajectoryDataObject = NULL selects the CSV
path. The same rules apply to saved local studies. CDM imports ignore
both local source arguments.

## Cohort definitions and saved studies

Fresh CDM imports stage definitions and their manifest in a temporary
folder. Downloaded definitions and the manifest are published after data
retrieval, cleaning and saving succeed. Failed downloads, queries or
empty eligible cohorts leave the previous working JSON files unchanged.
Existing local definitions and unrelated JSON files are not rewritten.

Successful CDM source overrides are retained for subsequent imports,
keeping target and state selectors and the ATLAS URL aligned with
clinical labels. Failed attempts retain the previous source settings.
Downloaded definitions use 0.json for the target and atlas\_\<id\>.json
for states. Repeated state IDs share a file without changing their
clinical labels.

Successful import captures the selected definition filenames and
contents. After fresh trajectory generation completes, these are saved
with the analysis settings in Settings/analysisSettings.rds. Only
version 4 snapshots are accepted. The snapshot contains no patient data.

Saved CDM runs write the embedded definitions to a new private folder
and use them to query fresh data. They do not read or replace working
JSON files or their manifest. A later fresh import cannot change the
definitions in an earlier completed snapshot. Saved local runs require
fresh local input. Neither saved mode accepts overrides to its restored
analysis rules.

## See also

[`vignette("a02_import", package = "Cohort2Trajectory")`](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.md),
[`vignette("a05_output", package = "Cohort2Trajectory")`](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
getDataForStudy(cdm = cdm, studyEnv = studyEnv)
} # }
```
