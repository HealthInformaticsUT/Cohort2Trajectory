# Build table for cohorts which shows the inclusion statistics

This function creates an inclusion table from getCohortsPatients()
function data

## Usage

``` r
getInclusionTable(cohortData, selectedCohorts, cohortLabels = NULL)
```

## Arguments

- cohortData:

  Output of getCohortsPatients()

- selectedCohorts:

  Vector of included cohort ID's

- cohortLabels:

  Labels for cohort

## Value

A dataframe with each selected cohort and its relation to the target
cohort
