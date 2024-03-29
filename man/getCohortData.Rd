% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importData.R
\name{getCohortData}
\alias{getCohortData}
\title{Get selected generated cohorts patient data}
\usage{
getCohortData(
  connection,
  dbms,
  resultsSchema,
  cdmSchema,
  selectedTarget,
  selectedStates,
  baseUrl = "http://localhost:8080/WebAPI",
  pathToResults = getwd()
)
}
\arguments{
\item{connection}{Connection to database}

\item{dbms}{Database management system (postgresql, mysql etc.)}

\item{resultsSchema}{Name of the OMOP results schema}

\item{selectedTarget}{The sample cohort ID}

\item{selectedStates}{Vector of included cohort ID's}
}
\value{
A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
}
\description{
This function queries all patient ID's in selected cohorts
}
\keyword{internal}
