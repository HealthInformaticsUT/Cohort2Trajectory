# Entirely synthetic two-person data, constructed in memory for database tests.
# No external dataset, patient record, download or persisted database is used.
fixtureExpression <- function(concept = 1001L) {
  list(cdmVersionRange = ">=5.0.0",
    PrimaryCriteria = list(
      CriteriaList = list(list(ConditionOccurrence = list(CodesetId = 0L))),
      ObservationWindow = list(PriorDays = 0L, PostDays = 0L),
      PrimaryCriteriaLimit = list(Type = "First")),
    ConceptSets = list(list(id = 0L, name = "Synthetic concept",
      expression = list(items = list(list(concept = list(CONCEPT_ID = concept),
        isExcluded = FALSE, includeDescendants = FALSE, includeMapped = FALSE))))),
    QualifiedLimit = list(Type = "First"), ExpressionLimit = list(Type = "First"),
    InclusionRules = list(), CensoringCriteria = list(),
    EndStrategy = list(DateOffset = list(DateField = "StartDate", Offset = 30L)),
    CollapseSettings = list(CollapseType = "ERA", EraPad = 0L))
}

writeFixtureDefinitions <- function(paths, concepts = c(1001L, 2001L, 3001L)) {
  for (i in seq_along(paths)) {
    dir.create(dirname(paths[i]), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(fixtureExpression(concepts[i]), paths[i], auto_unbox = TRUE)
  }
}

fixtureCdm <- function(con, name = "Synthetic two-person fixture") {
  testthat::skip_if_not_installed("CirceR")
  testthat::skip_if_not_installed("SqlRender")
  day <- as.Date("2020-01-01")
  input <- list(
    person = data.frame(person_id = 1:2, year_of_birth = c(1950L, 1960L),
      gender_concept_id = c(8507L, 8532L), race_concept_id = 0L, ethnicity_concept_id = 0L),
    observation_period = data.frame(observation_period_id = 1:2, person_id = 1:2,
      observation_period_start_date = day - 100L, observation_period_end_date = day + 100L,
      period_type_concept_id = 44814724L),
    condition_occurrence = data.frame(condition_occurrence_id = 1:6,
      person_id = rep(1:2, each = 3), condition_concept_id = rep(c(1001L, 2001L, 3001L), 2),
      condition_start_date = day + rep(c(0L, 2L, 7L), 2),
      condition_end_date = day + rep(c(0L, 2L, 7L), 2), condition_type_concept_id = 32817L),
    concept = data.frame(concept_id = c(1001L, 2001L, 3001L),
      concept_name = c("Synthetic target", "Synthetic state A", "Synthetic state B"),
      domain_id = "Condition", vocabulary_id = "SNOMED", concept_class_id = "Clinical Finding",
      standard_concept = "S", concept_code = as.character(c(1001L, 2001L, 3001L)),
      valid_start_date = as.Date("1970-01-01"), valid_end_date = as.Date("2099-12-31"),
      invalid_reason = NA_character_))
  cdm <- omopgenerics::emptyCdmReference(name, cdmVersion = "5.4")
  for (table in c(names(input), "concept_ancestor", "concept_relationship", "vocabulary")) {
    cdm <- omopgenerics::emptyOmopTable(cdm, table)
    template <- as.data.frame(cdm[[table]])
    if (table %in% names(input)) {
      rows <- template[rep(NA_integer_, nrow(input[[table]])), , drop = FALSE]
      for (field in names(input[[table]])) rows[[field]] <- input[[table]][[field]]
    } else rows <- template
    DBI::dbWriteTable(con, table, rows)
  }
  CDMConnector::cdmFromCon(con, cdmName = name, cdmVersion = "5.4",
                          cdmSchema = "main", writeSchema = "main")
}
