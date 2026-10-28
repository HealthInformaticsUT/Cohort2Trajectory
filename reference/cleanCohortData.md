# This function eliminates patients which do not fulfill the inclusion criteria

This function eliminates patients which do not fulfill the inclusion
criteria

## Usage

``` r
cleanCohortData(cohortData, mandatoryStates, outOfCohortAllowed = FALSE)
```

## Arguments

- cohortData:

  Imported cohort data

- mandatoryStates:

  States which have to be present in the trajectory, otherwise dropped#'

- outOfCohortAllowed:

  boolean whether the patient trajectory can surpass the target cohort's
  observation-period

## Value

A dataframe with selected patients. Columns: cohort_definition_id,
subject_id, cohort_start_date, cohort_end_date
