# Bound cohort intervals and apply mandatory-state requirements

Bound cohort intervals and apply mandatory-state requirements

## Usage

``` r
cleanCohortData(cohortData, mandatoryStates, outOfCohortAllowed = FALSE)
```

## Arguments

- cohortData:

  Imported cohort data

- mandatoryStates:

  States required for each retained patient.

- outOfCohortAllowed:

  boolean whether the patient trajectory can surpass the target cohort's
  observation-period

## Value

A dataframe with selected patients. Columns: cohort_definition_id,
subject_id, cohort_start_date, cohort_end_date
