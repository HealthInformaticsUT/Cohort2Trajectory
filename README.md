# Cohort2Trajectory

Cohort2Trajectory turns cohort intervals into patient trajectories for analysis.
A target cohort defines each patient's observation period; state cohorts define
events or periods within it. When states overlap, you choose which takes
priority or whether to represent their combination.

The package accepts an R data frame, a CSV file or cohorts generated in an
OMOP Common Data Model database through CDMConnector. It produces fixed-length
cycles or continuous intervals, with explicit rules for state selection,
allowed transitions, gaps and absorbing states. Study settings can be reused
with fresh data at another site.

## A small example

This example needs no database. In local input, `"0"` identifies the target
cohort and other labels identify states. Intervals include both endpoint dates.

```r
library(Cohort2Trajectory)

cohorts <- data.frame(
  subject_id = rep("001", 3),
  cohort_definition_id = c("0", "Stable", "Complication"),
  cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7),
  cohort_end_date = as.Date("2020-01-21")
)

study <- cohort2TrajectoryConfiguration(
  studyName = "example",
  pathToStudy = tempdir(),
  useCDM = FALSE,
  trajectoryDataObject = cohorts,
  stateCohortLabels = c("Stable", "Complication"),
  stateCohortPriorityOrder = c("Complication", "Stable"),
  stateSelectionType = "Priority",
  trajectoryType = "Discrete",
  cycleLength = 7
)

getDataForStudy(studyEnv = study)
trajectories <- createTrajectories(studyEnv = study)
trajectories
```

The patient is Stable in the first week and in Complication for the next two.
The returned table also contains `START` and `EXIT` boundary rows. Input and
output files are saved under `file.path(tempdir(), "example")`.

## Choosing a trajectory

A discrete trajectory selects one state per cycle. Its label describes the
selection for that cycle, not necessarily every day within it. The final cycle
can be shorter than `cycleLength`. A continuous trajectory retains selected
interval dates and can resolve overlaps by priority.

You supply the clinical definitions: an episode, a history of that episode and
recovery need their own cohort definitions when the analysis distinguishes them.
`OUT OF COHORT` marks a discrete cycle with no selected state. `START` and
`EXIT` mark observation boundaries.

Use `mergeStates = TRUE` to represent overlapping states together.
`mergedState()` can change a combination's destinations, absorbing status or
display label. Membership is stored separately from labels.

## Documentation

The [documentation site](https://healthinformaticsut.github.io/Cohort2Trajectory/)
contains the function reference and these guides, also available as package
vignettes:

- [Getting started](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a00_introduction.html): local input and the three-step workflow.
- [Configure a study](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a01_configuration.html): observation bounds, selection and merged states.
- [Import cohort data](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a02_import.html): CSV, data frames, ATLAS and local JSON definitions.
- [Create trajectories](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a03_trajectories.html): transition rules, gaps and saved-study reruns.
- [A weekly worked example](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a04_example.html): pneumonia history and two death states.
- [Read the output](https://healthinformaticsut.github.io/Cohort2Trajectory/articles/a05_output.html): columns, files and downstream analysis.

For CDM input, the database account needs read access to the required CDM and
vocabulary tables and permission to create and drop tables in the write schema.
ATLAS authentication headers can be supplied at import. Exported JSON
definitions can be used without an ATLAS connection.

## Reuse and analysis

A completed run saves its settings in `Settings/analysisSettings.rds`, including
the imported cohort JSON for CDM studies. Copy that folder to rerun the study
with fresh data. Keep `Data` private: it contains patient records.

Returned trajectories carry their cycle length, ordered state labels and
absorbing states for use by
[TrajectoryMarkovAnalysis](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis).
Use `saveRDS()` to preserve both the rows and these definitions. CSV contains
only the rows, so downstream tools need the definitions supplied separately.

Related packages:
[CohortContrast](https://github.com/HealthInformaticsUT/CohortContrast) and
[TrajectoryViz](https://github.com/HealthInformaticsUT/TrajectoryViz).

## Citation and contact

Haug M, Oja M, Pajusalu M, et al. *Markov modeling for cost-effectiveness using
federated health data network*. Journal of the American Medical Informatics
Association, 2024. [doi:10.1093/jamia/ocae044](https://doi.org/10.1093/jamia/ocae044).
Record the package version and settings used for your analysis.

Apache License 2.0. Developed by the University of Tartu Health Informatics
research group. Contact [Markus Haug](mailto:markus.haug@ut.ee).
