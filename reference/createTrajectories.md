# Generate patient trajectories from imported cohort intervals

Apply the configured selection, transition and gap rules to a successful
study import. See
[`cohort2TrajectoryConfiguration`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md)
for these rules and the required state definitions.

## Usage

``` r
createTrajectories(
  cdm = NULL,
  studyEnv = NULL,
  trajectoryType = studyEnv$trajectoryType,
  runSavedStudy = studyEnv$runSavedStudy,
  oocFix = studyEnv$oocFix,
  outOfCohortAllowed = studyEnv$outOfCohortAllowed,
  cycleLength = studyEnv$cycleLength,
  stateCohortLabels = studyEnv$stateCohortLabels,
  stateCohortPriorityOrder = studyEnv$stateCohortPriorityOrder,
  stateSelectionType = studyEnv$stateSelectionType,
  stateCohortAbsorbing = studyEnv$stateCohortAbsorbing,
  stateCohortMandatory = studyEnv$stateCohortMandatory,
  allowedStatesList = studyEnv$allowedStatesList,
  useCDM = studyEnv$useCDM,
  pathToStudy = studyEnv$pathToStudy,
  batchSize = studyEnv$batchSize,
  mergedStateOverrides = studyEnv$mergedStateOverrides,
  stopAfterAbsorbingEpisode = studyEnv$stopAfterAbsorbingEpisode
)
```

## Arguments

- cdm:

  A CDM reference created with CDMConnector, required when joining
  demographics with `useCDM = TRUE`.

- studyEnv:

  Study environment returned by
  [`cohort2TrajectoryConfiguration`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md).
  Its settings supply the defaults.

- trajectoryType:

  `"Discrete"` or `"Continuous"`, ignoring case and surrounding
  whitespace. A choice is required for generation.

- runSavedStudy:

  Whether to use restored saved-study settings. Import fresh data first.
  Saved runs do not accept analysis-setting overrides.

- oocFix:

  Discrete gap rule: `"None"`, `"OUT OF COHORT"`, `"Last present state"`
  or a fallback state label. See
  [`cohort2TrajectoryConfiguration`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md)
  for transition and label rules.

- outOfCohortAllowed:

  Whether retained state records may extend the trajectory beyond the
  target interval's end. The target start is unchanged.

- cycleLength:

  Positive whole-number cycle length in days, within R's integer range.
  Required when generating discrete trajectories. It may be `NULL`
  during configuration. The final cycle is shortened at the observation
  boundary.

- stateCohortLabels:

  Character vector of state labels. May be omitted when
  `atlasStateCohorts` is named. If both are supplied, the labels must
  match its names in order. Labels must be nonempty, non-missing and
  unique even when ignoring case. `"0"`, `"START"`, `"EXIT"` and
  `"OUT OF COHORT"` are reserved, including case variants. Labels may
  contain spaces or punctuation. They are separate from database cohort
  names and definition filenames.

- stateCohortPriorityOrder:

  State labels from highest to lowest priority. All states within the
  observation bounds must be listed for discrete selection or continuous
  `"Priority"` selection. For a merged study, `NULL` uses a
  deterministic order of original states.

- stateSelectionType:

  `"First"`, `"Overlap"` or `"Priority"`, ignoring case and surrounding
  whitespace. A choice is required for generation.

- stateCohortAbsorbing:

  State labels that end follow-up when selected. `NULL` or
  `character(0)` means none. For an unmerged study, use exact configured
  or observed labels, or a named discrete fallback. Configured states
  may be absent from the current data. For merged studies, supply
  original state labels here and use `mergedStateOverrides` for
  exceptions.

- stateCohortMandatory:

  Mandatory states used by the successful import. Rerun
  [`getDataForStudy`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
  to change them. Order and duplicates do not matter. `NULL` and
  `character(0)` both mean no requirements.

- allowedStatesList:

  Named list of allowed destinations, for example
  `list(A = c("B", "C"))`. Source names must be unique, nonempty and
  non-missing. Put all destinations for one source in a single entry. An
  empty list allows all transitions. `character(0)` or `NULL` in an
  individual entry permits no destinations. Nonmerged generation checks
  destinations against configured and observed states and any named
  discrete fallback, ignoring case. Unknown destinations are rejected
  before output files are written. Configured states may be unobserved.

- useCDM:

  Whether to join age and gender from the supplied CDM. Otherwise, `age`
  and `gender_concept_id` contain `NA`.

- pathToStudy:

  Directory containing the study folder. Generation uses the location
  stored in `studyEnv`.

- batchSize:

  Maximum patients per generation batch. Supply one positive whole
  number within R's integer range. The default is 1000.

- mergedStateOverrides:

  List of
  [`mergedState`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/mergedState.md)
  definitions. Requires `mergeStates = TRUE`. Supply at most one
  definition per membership set. Omitted properties retain inherited
  values.

- stopAfterAbsorbingEpisode:

  Whether a selected absorbing state also excludes later episodes for
  that patient. The default, `FALSE`, lets later episodes start afresh.
  An absorbing state always ends its own episode.

## Value

A data frame of class `c2t_trajectories`, also saved as a CSV in the
study's `Data` directory. Printing shows counts and a short preview; use
`as.data.frame(result)` for the ordinary data-frame display. See
[`print.c2t_trajectories`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/print.c2t_study.md)
for printing options and the output vignette for the complete schema.
The `trajectory_definition` attribute records the effective trajectory
type, cycle length, ordered clinical labels and absorbing labels.
TrajectoryMarkovAnalysis reads first-period definitions directly.
All-period output uses version 2 metadata with episode identity and time
origin, requiring an episode-aware downstream reader. Labels and
absorbing status reflect merged-state overrides; configured but
unobserved states remain declared.
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) preserves this
attribute; CSV output contains only rows and columns.

## Boundaries and timing

Target intervals, labelled `"0"` in cleaned data, supply the start and
end boundaries. The import's `targetEpisodeHandling` selects the first
interval or all episodes. Patients without clinical state records remain
eligible unless mandatory-state requirements exclude them. Discrete
cycles include both endpoint dates, and the final cycle is shortened at
the observation boundary. A selected absorbing state stops generation
after its cycle. Its original onset date does not shorten that cycle.

Continuous trajectories end at the retained absorbing interval's end
date. Earlier overlapping intervals are capped at that endpoint. `START`
and `EXIT` are boundary markers and add no observed time. `EXIT` is on
the day after the last cycle or continuous endpoint. Its
`time_in_cohort` is elapsed years from trajectory start, using 365.25
days per year.

`outOfCohortAllowed = TRUE` can extend the endpoint using retained state
records; it cannot recover records removed during import. A discrete
state label represents the selected state for a cycle, without
preserving exact event dates or ordering within that cycle.

## Study workflow

Run
[`getDataForStudy`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/getDataForStudy.md)
successfully before generation. A failed import blocks generation until
a later import succeeds; previous patient files are preserved and are
not reused for the failed attempt. The trajectory CSV is written after
all patient batches finish, preserving a previous complete file if a
batch fails. Unknown transition destinations are rejected before
generation writes output files.

A completed fresh run saves `Settings/analysisSettings.rds` in version 6
format. Versions 4 and 5 also load with first-period behavior. See
[`cohort2TrajectoryConfiguration`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md)
for saving and sharing rules.

## Labels and identifiers

State IDs are assigned once per study. In unmerged studies, `START` has
ID 1, followed by configured states in their supplied order, additional
input and discrete gap labels in alphabetical order, then `EXIT`.
Configured states keep their IDs when absent from a batch. Merged
studies use dictionary order and add `state_key` to the output. Their
`stateDictionary` is available in `studyEnv` and
`Data/stateDictionary.rds`. Use keys and membership for comparison
between sites; state IDs are local to the study.

Patient identifiers are preserved. File and CDM input preserve large IDs
and leading zeros as text. Canonical numeric IDs below `2^53` retain
numeric output. Read `subject_id` as character when importing exported
CSVs to preserve long or zero-prefixed identifiers.

## See also

[`cohort2TrajectoryConfiguration`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md),
[`vignette("a03_trajectories", package = "Cohort2Trajectory")`](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.md),
[`vignette("a05_output", package = "Cohort2Trajectory")`](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.md)

## Examples

``` r
cohorts <- data.frame(
  subject_id = rep("001", 3),
  cohort_definition_id = c("0", "Stable", "Complication"),
  cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7),
  cohort_end_date = as.Date("2020-01-21")
)
folder <- tempfile("c2t-example-")
study <- cohort2TrajectoryConfiguration(
  studyName = "example", pathToStudy = folder,
  useCDM = FALSE, trajectoryDataObject = cohorts,
  stateCohortLabels = c("Stable", "Complication"),
  stateCohortPriorityOrder = c("Complication", "Stable"),
  stateSelectionType = "Priority", cycleLength = 7
)
getDataForStudy(studyEnv = study)
#> ℹ Importing data ...
#> ✔ Importing data ... [3ms]
#> 
#> ✔ Get cohort data success!
#> ℹ Cleaning data ...
#> ✔ Data cleaning completed!
#> ℹ Cleaning data ...

#> ℹ Saved cleaned data /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T//RtmpUjaIV7/c2t-example-b47c4c341355/example/Data/importedDataCleaned.csv
#> ℹ Cleaning data ...

#> ✔ Cleaning data ... [25ms]
#> 
trajectories <- createTrajectories(studyEnv = study)
#> ℹ Creating batch 1!!!
#> ℹ Saved trajectory dataframe: /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T//RtmpUjaIV7/c2t-example-b47c4c341355/example/Data/patientDataDiscrete.csv
#> ℹ Saved settings to: /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T//RtmpUjaIV7/c2t-example-b47c4c341355/example/Settings/analysisSettings.rds
#> ✔ Trajectories generated!
trajectories[c("subject_id", "state_label", "state_start_date", "state_end_date")]
#> <Cohort2Trajectory trajectories>
#> Patients: 1 | Clinical states present: 2 | Rows: 5 
#> Boundary rows (START/EXIT): 2 | Unresolved rows (OUT OF COHORT): 0 
#> Preview: 5 of 5 rows; 4 of 4 columns
#>   subject_id  state_label state_start_date state_end_date
#> 1        001        START       2020-01-01     2020-01-01
#> 2        001       Stable       2020-01-01     2020-01-07
#> 3        001 Complication       2020-01-08     2020-01-14
#> 4        001 Complication       2020-01-15     2020-01-21
#> 5        001         EXIT       2020-01-22     2020-01-22
#> Use as.data.frame(x) to inspect every column.
unlink(folder, recursive = TRUE)
```
