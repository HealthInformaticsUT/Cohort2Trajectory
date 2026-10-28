test_that("successful CDM source overrides remain coherent on the next import", {
  root <- tempfile("c2t-import-settings-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  requested <- integer()
  endpoints <- character()
  local_mocked_bindings(
    getCohortDefinition = function(cohortId, baseUrl) {
      requested <<- c(requested, cohortId)
      endpoints <<- c(endpoints, baseUrl)
      list(expression = list(definition = as.character(cohortId)))
    }, .package = "ROhdsiWebApi"
  )
  local_mocked_bindings(
    readCohortSet = function(path) list(),
    generateCohortSet = function(cdm, cohortSet, name) cdm,
    .package = "CDMConnector"
  )
  raw <- data.frame(
    cohort_definition_id = c(1L, 2L), subject_id = "patient",
    cohort_start_date = as.Date("2020-01-01"), cohort_end_date = as.Date("2020-01-07")
  )
  study <- cohort2TrajectoryConfiguration(
    pathToStudy = root, studyName = "sources", baseUrl = "https://old.example/WebAPI",
    atlasTargetCohort = 101, atlasStateCohorts = 202,
    stateCohortLabels = "A", stateCohortPriorityOrder = "A",
    stateSelectionType = "Priority", lengthOfStay = 7
  )
  newUrl <- "https://new.example/WebAPI"
  getDataForStudy(cdm = list(cohort = raw), studyEnv = study, baseUrl = newUrl,
                  atlasTargetCohort = 303, atlasStateCohorts = 404,
                  stateCohortLabels = "B", stateCohortPriorityOrder = "B",
                  allowedStatesList = list(B = "B"))
  expected <- list(atlasTargetCohort = 303, atlasStateCohorts = 404, baseUrl = newUrl)
  expect_identical(mget(names(expected), study, inherits = FALSE), expected)

  requested <- integer()
  endpoints <- character()
  getDataForStudy(cdm = list(cohort = raw), studyEnv = study)
  expect_identical(requested, c(303L, 404L))
  expect_identical(endpoints, rep(newUrl, 2))
  expect_identical(createTrajectories(studyEnv = study, useCDM = FALSE)$state_label,
                   c("START", "B", "EXIT"))

  expect_warning(getDataForStudy(
    cdm = list(cohort = raw), studyEnv = study, baseUrl = "https://failed.example/WebAPI",
    atlasTargetCohort = 505, atlasStateCohorts = 606, stateCohortMandatory = "Missing"
  ), "No patients left after cleaning")
  expect_identical(mget(names(expected), study, inherits = FALSE), expected)
  expect_false(study$.dataReady)
})
