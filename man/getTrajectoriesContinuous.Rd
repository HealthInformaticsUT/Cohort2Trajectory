% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importData.R
\name{getTrajectoriesContinuous}
\alias{getTrajectoriesContinuous}
\title{Prepare patientData dataframe for msm package}
\usage{
getTrajectoriesContinuous(
  connection,
  patientData,
  stateSelection,
  statePriorityVector,
  absorbingStates = NULL,
  studyName = "",
  pathToResults = paste(getwd(), "/tmp", sep = ""),
  addPersonalData = TRUE,
  allowedStatesList = list()
)
}
\arguments{
\item{connection}{Connection to the database}

\item{patientData}{Object of class data.frame with columns SUBJECT_ID, STATE_LABEL, STATE_START_DATE, STATE_END_DATE}

\item{stateSelection}{Selection of the type of ordering}

\item{statePriorityVector}{All states in prioritized order}

\item{absorbingStates}{Absorbing states in dataframe}

\item{addPersonalData}{Logical, indicating whether or not to add personal data for each patient}
}
\value{
A dataframe ready for using msm package
}
\description{
This function prepares patientData dataframe for msm package
}
\keyword{internal}
