# Generate cohort set CSV

internal function for generating cohort set CSV for CDMConnector
generateCohorts

## Usage

``` r
generateCohortSetCSV(selectedStates, stateCohortLabels, pathToStudy, studyName)
```

## Arguments

- selectedStates:

  all states to be included in db query

- stateCohortLabels:

  user inserted labels for non target states

- pathToStudy:

  path to folder that contains study folder

- studyName:

  name of the study directory

## Value

cohortSetCSV df
