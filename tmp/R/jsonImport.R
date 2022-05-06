#' Get data from OMOP CDM specified in imported JSON files
#'
#' This function outputs a dataframe with columns SUBJECT_ID, COHORT_DEFINITION_ID, COHORT_START_DATE, COHORT_END_DATE
#' @param connection Connection to database
#' @param connectionDetails An object of class connectionDetails as created by the DatabaseConnector::createConnectionDetails function.
#' @param jsons JSONs from which cohorts will be created
#' @param names Names of the cohorts
#' @param cdmDataSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmTempSchema Schema for temporary tables
#' @param studyName Customized name for the study
#' @keywords internal
getJSONData = function(connection,
                       connectionDetails,
                       jsons,
                       names,
                       cdmDataSchema,
                       cdmTempSchema,
                       studyName) {
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(jsons)) {
    cohortJson <- jsons[i]
    cohortName <- names[i]
    
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <-
      CirceR::buildCohortQuery(cohortExpression,
                               options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <-
      rbind(
        cohortsToCreate,
        data.frame(
          cohortId = i,
          cohortName = cohortName,
          sql = cohortSql,
          stringsAsFactors = FALSE
        )
      )
  }
  # Create the cohort tables to hold the cohort generation results
  cohortTableNames <-
    CohortGenerator::getCohortTableNames(cohortTable = studyName)
  CohortGenerator::createCohortTables(
    connection = connection,
    cohortDatabaseSchema = cdmTempSchema,
    cohortTableNames = cohortTableNames
  )
  # Generate the cohorts
  CohortGenerator::generateCohortSet(
      connection = connection,
      cdmDatabaseSchema = cdmDataSchema,
      cohortDatabaseSchema = cdmTempSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = cohortsToCreate
    )
  
  sql <-
    loadRenderTranslateSql(
      dbms = connectionDetails$dbms,
      "SELECT * FROM @cdmTempSchema.@studyName",
      cdmTempSchema = cdmTempSchema,
      studyName = studyName
    )
  data = DatabaseConnector::querySql(connection, sql)
  # Apply state names
  names = c("0", names)
  data$COHORT_DEFINITION_ID = plyr::mapvalues(
    x = data$COHORT_DEFINITION_ID,
    from = 1:length(names),
    to = names,
    warn_missing = FALSE
  )
  
  data = dplyr::select(data,
                       SUBJECT_ID,
                       COHORT_DEFINITION_ID,
                       COHORT_START_DATE,
                       COHORT_END_DATE)
  
  return(data)
}

