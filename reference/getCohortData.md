# Get selected generated cohorts patient data

This function queries all patient ID's in selected cohorts

## Usage

``` r
getCohortData(
  cdm,
  studyEnv,
  selectedTarget,
  selectedStates,
  baseUrl = "http://localhost:8080/WebAPI",
  pathToStudy = studyEnv$pathToStudy,
  studyName = studyEnv$studyName,
  stateCohortLabels = NULL
)
```

## Arguments

- cdm:

  common data model reference object

- studyEnv:

  environment created with cohort2TrajectoryConfiguration

- selectedTarget:

  The sample cohort ID

- selectedStates:

  Vector of included cohort ID's

- baseUrl:

  the base URL for the WebApi instance

- pathToStudy:

  path to folder which contains folder named after studyName

- studyName:

  name chosen for study, is used as a folder name

- stateCohortLabels:

  Vector of the customized labels of the state cohorts

## Value

A dataframe with selected patients. Columns: cohort_definition_id,
subject_id, cohort_start_date, cohort_end_date
