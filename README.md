
# Cohort2Trajectory

NOTE: After refactoring to CDMConnector the package execution code has significantly changed. You can find the old release v1.1.8 [here](https://github.com/HealthInformaticsUT/Cohort2Trajectory/releases/tag/v1.1.8). (January 2025)

# Introduction

Package Cohort2Trajectory creates patient orientated treatment trajectories from cohorts defined in OHDSI ATLAS. The package can be used with access to OMOP CDM database.
The package creates discrete and continuous time trajectories (and outputs them as .csv file) which describe patients' treatment through time.

For instructions and documentation please see the package's [wiki](https://healthinformaticsut.github.io/Cohort2Trajectory/) page.

# Features

**Sharing the study**: A successful import and trajectory-generation run saves
the complete analysis rules in `Settings/analysisSettings.rds`. This is the only
settings file written or read by the package. The snapshot preserves import
and generation settings separately, including allowed transitions, optional
state lists, cycle length and merging options.

Use readable names for `trajectoryType` (`"Discrete"` or `"Continuous"`) and
`stateSelectionType` (`"First"`, `"Overlap"` or `"Priority"`) in both configuration
and generation. Names ignore case and surrounding whitespace. The configuration
and new RDS snapshots store the canonical names, not numeric engine codes.
Legacy numeric arguments still work but warn with the replacement name.
Existing non-merged version-1 and version-2 RDS snapshots are converted in memory
when loaded, without modifying the original file. Removed threshold settings
produce a warning. New snapshots use version 3 and require the updated package
when shared. Legacy merged studies must be reconfigured from their original
state definitions because their saved labels do not reliably identify membership.

## Combining overlapping states

Use `mergeStates = TRUE` to represent simultaneous original states as a single
combined state over their exact overlapping interval. No threshold is used.
For example, A on days 1-10 and B on days 5-7 produce A on days 1-4, A+B on
days 5-7, and A on days 8-10. Composition follows observation-boundary and
mandatory-state filtering, and precedes weekly or other discretisation.
Two events on different days of the same week are not automatically combined.

Membership is stored explicitly, never recovered from labels. An original state
called `A+B` remains an original state. It is not confused with the combination
of A and B. Only observed combinations and explicitly specified definitions are
registered, without enumerating permutations of all possible combinations.

By default:

- Permitted destinations are inherited by union across the source members.
  Thus A+B can enter C when A allows C, even if B does not.
- Entering a composite destination requires permission for every newly added
  member. A permission for C alone does not allow entry into C+D.
- Self-transitions and transitions only removing members require permission for
  the retained destination members. Empty original-state destination lists do
  not silently acquire self-transitions.
- A combination is absorbing if any member is absorbing. Absorbing behaviour
  takes precedence over permitted outgoing transitions.
- Priority follows the highest-priority member, with larger combinations first
  at equal priority. With no supplied priority order, original labels supply a
  deterministic order. The priority rule selects among intervals within a
  discrete cycle, not among duplicate original and combined patient-time.

Use `mergedState()` only for exceptions:

```r
study <- cohort2TrajectoryConfiguration(
  useCDM = FALSE,
  trajectoryDataObject = input,
  stateCohortLabels = c("A", "B", "C"),
  stateCohortPriorityOrder = c("A", "B", "C"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  lengthOfStay = 7,
  stateCohortAbsorbing = "A",
  allowedStatesList = list(A = "C", B = character(0), C = "C"),
  mergeStates = TRUE,
  mergedStateOverrides = list(
    mergedState(c("A", "B"), absorbing = FALSE, label = "Combined")
  )
)
getDataForStudy(studyEnv = study)
trajectories <- createTrajectories(studyEnv = study)
study$stateDictionary
```

Omitted override properties inherit. `allowedTransitions = character(0)` means
no destinations, and `absorbing = FALSE` explicitly removes inherited absorbing
status. A supplied destination list replaces, rather than extends, inheritance.
For example, `allowedTransitions = list("C", c("C", "D"))` explicitly permits
both C alone and the C+D combination. Members always refer to original states.
Names containing punctuation need no escaping or manual numeric remapping.

The configuration and saved rules retain original-state definitions and
structured overrides. Resolved memberships, labels, absorbing flags and allowed
destination keys are available in `study$stateDictionary` and
`Data/stateDictionary.rds`. Merged trajectory outputs contain readable
`state_label` values and a stable `state_key`. Numeric `state_id` values are local
to the run and should not be used to identify combinations across databases.
Generated display-label collisions are disambiguated without changing membership.
Bounded original-state intervals are retained in `Data/sourceIntervals.rds`.
This file contains patient-level data and must not be included when sharing a study.

Legacy named `mergeThreshold` arguments warn and are ignored. The previous native
merger never used the threshold. `updateAutomaticallyForMergedStates` also warns
when explicitly supplied and is ignored because structured rules are always
resolved automatically. Merging results can differ from older versions because
overlapping patient-time is now resolved once and rule inheritance is explicit.

## Rerunning a study on another database

Copy the study's **JSON** and **Settings** folders into a study folder at the
new location. Do not share **Data**, which contains patient-level records.
Use the updated package at both sites. With a CDMConnector connection to the
destination database called `newCdm`, run:

```r
study <- cohort2TrajectoryConfiguration(
  studyName = "MyStudy",
  pathToStudy = "/new/location", # Contains the MyStudy folder
  runSavedStudy = TRUE
)
getDataForStudy(cdm = newCdm, studyEnv = study)
trajectories <- createTrajectories(cdm = newCdm, studyEnv = study)
```

No ATLAS connection or re-entry of analysis settings is needed. The saved
mapping identifies which JSON files define the target and source states,
independently of any merged output states. The package regenerates cohorts and
imports data from the new database before allowing trajectory generation.
Connections, local paths and patient data are not part of the settings snapshot.
Database permissions and adequate local data coverage are still required.

New ATLAS downloads keep the target in `0.json` and state definitions in
`atlas_<id>.json`. Clinical labels are not filenames, and repeated ATLAS IDs
can share a definition file. Existing saved studies retain their stored mapping.

For non-CDM studies, add `useCDM = FALSE` and provide fresh input through
`pathToData` or `trajectoryDataObject`. Batch size can be changed without changing
the analysis rules. Saved runs reject explicit analysis-setting overrides.
Configure a new study if you want different rules. To inspect the saved settings:

```r
settings <- readRDS("/new/location/MyStudy/Settings/analysisSettings.rds")
str(settings)
```

Older CSV-only folders do not contain allowed-transition or merging rules.
Rerun the original configured import and trajectory generation once with the
updated package to create the complete snapshot before moving the study.
Existing settings CSV files are left untouched and ignored. Patient-output CSVs
and the cohort-definition manifest remain unchanged.

# Technology

Cohort2Trajectory is an R package, with some functions implemented in C++.

For running the package with OMOP CDM, the user must have: 

1. Permissions to select on ohdsi cdm data schema.

2. Permissions to select on ohdsi cdm results schema.

3. Permissions to select, create and insert on ohdsi temp/scratch schema.

# License

Cohort2Trajectory is licensed under Apache License 2.0

# Development

Cohort2Trajectory is being developed in R Studio.

Feel free to contact the maintainer with questions: [markus.haug\@ut.ee](mailto:markus.haug@ut.ee){.email}

# See also 

[HealthInformaticsUT/CohortContrast](https://github.com/HealthInformaticsUT/CohortContrast)

[HealthInformaticsUT/TrajectoryMarkovAnalysis](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis)

[HealthInformaticsUT/TrajectoryViz](https://github.com/HealthInformaticsUT/TrajectoryViz)

# Publications

Markus Haug, Marek Oja, Maarja Pajusalu, Kerli Mooses, Sulev Reisberg, Jaak Vilo, Antonio Fernández Giménez, Thomas Falconer, Ana Danilović, Filip Maljkovic, Dalia Dawoud, Raivo Kolde, Markov modeling for cost-effectiveness using federated health data network, Journal of the American Medical Informatics Association, 2024;, ocae044, <https://doi.org/10.1093/jamia/ocae044>

# Citation

Haug, M.
(2023, May).
Cohort2Trajectory, v1.1.3.
GitHub.
<https://github.com/HealthInformaticsUT/Cohort2Trajectory/releases/tag/v1.1.8>

# Acknowledgements

Research group of Health-Informatics in University of Tartu <https://health-informatics.cs.ut.ee/>
