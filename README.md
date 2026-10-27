
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
