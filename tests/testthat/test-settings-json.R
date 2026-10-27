test_that("saved studies load only cohort JSON files and keep states without a target file", {
  path <- tempfile("c2t-json-")
  dir.create(file.path(path, "study", "Settings"), recursive = TRUE)
  dir.create(file.path(path, "study", "JSON"))
  on.exit(unlink(path, recursive = TRUE))
  settings <- data.frame(
    studyName = "study", trajectoryType = "Discrete", trajectoryStates = "A",
    priorityOrder = "A", stateSelectionType = 3, absorbingStates = "",
    mandatoryStates = "", lengthOfStay = 7, outOfCohortAllowed = FALSE,
    outOfCohortFix = "None"
  )
  write.csv(settings, file.path(path, "study", "Settings", "trajectorySettings.csv"),
            row.names = FALSE)
  jsonDir <- file.path(path, "study", "JSON")
  writeLines('{"name":"target"}', file.path(jsonDir, "0.json"))
  writeLines('{"name":"A"}', file.path(jsonDir, "A.json"))
  # The package itself writes this manifest beside the cohort definitions.
  writeLines("cohortId,cohortName,jsonPath", file.path(jsonDir, "CohortsToCreate.csv"))
  loaded <- loadSettings("study", path)
  expect_identical(loaded$stateNamesJSON, "A")
  expect_identical(loaded$insertedJSONs, '{"name":"A"}')
  expect_identical(loaded$targetJSON, '{"name":"target"}')

  unlink(file.path(jsonDir, "0.json"))
  loaded <- loadSettings("study", path)
  expect_identical(loaded$stateNamesJSON, "A")
  expect_identical(loaded$insertedJSONs, '{"name":"A"}')
  expect_identical(loaded$targetJSON, "")
})
