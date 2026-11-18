test_that("CDM import preserves labels, caller tables and nested definitions", {
  root <- tempfile("c2t-cohort-names-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  labels <- c("Recorded pneumonia", "target", "Recorded_pneumonia",
              "30-day recovery", "(Other care)", "\u00c4ge faas")
  definitions <- c("clinical", "targets/stroke", rep("clinical", 4))
  study <- cohort2TrajectoryConfiguration(
    studyName = "names", pathToStudy = root, baseUrl = NULL,
    atlasTargetCohort = "targets/stroke", atlasStateCohorts = definitions,
    stateCohortLabels = labels, stateCohortPriorityOrder = labels,
    stateSelectionType = "Priority", cycleLength = 7
  )
  writeFixtureDefinitions(file.path(root, "names", "JSON",
    c("targets/stroke.json", "clinical.json")), c(1001L, 2001L))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cdm <- fixtureCdm(con)
  sentinel <- data.frame(value = 123L)
  DBI::dbWriteTable(con, "cohort", sentinel)
  DBI::dbWriteTable(con, "test", sentinel)
  tables <- DBI::dbListTables(con)
  getDataForStudy(cdm = cdm, studyEnv = study)
  data <- .readPatientCsv(.cleanedDataPath(root, "names"))
  expect_setequal(data$cohort_definition_id, c("0", labels))
  records <- function(label) {
    rows <- data[data$cohort_definition_id == label,
                 c("subject_id", "cohort_start_date", "cohort_end_date")]
    dplyr::arrange(rows, subject_id, cohort_start_date, cohort_end_date)
  }
  expect_equal(records("0"), records("target"))
  expect_equal(records("Recorded pneumonia"), records("Recorded_pneumonia"))
  for (label in labels[4:6]) expect_equal(records(label), records("Recorded pneumonia"))
  set.seed(1)
  rng <- .Random.seed
  trajectories <- createTrajectories(cdm = cdm, studyEnv = study)
  expect_identical(.Random.seed, rng)
  expect_setequal(trajectories$subject_id, 1:2)
  expect_equal(DBI::dbReadTable(con, "cohort"), sentinel)
  expect_equal(DBI::dbReadTable(con, "test"), sentinel)
  expect_true(all(tables %in% DBI::dbListTables(con)))
  expect_false(any(grepl("^c2t_", DBI::dbListTables(con))))
  expect_true(DBI::dbIsValid(con))
})
