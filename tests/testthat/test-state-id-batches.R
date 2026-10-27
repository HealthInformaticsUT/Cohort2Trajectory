make_state_id_fixture <- function() {
  data.frame(
    subject_id = c(1L, 1L, 2L, 2L, 3L, 3L, 3L),
    cohort_definition_id = c("0", "A", "0", "B", "0", "B", "A"),
    cohort_start_date = as.Date(c(
      "2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02",
      "2020-01-01", "2020-01-02", "2020-01-09"
    )),
    cohort_end_date = as.Date(c(
      "2020-01-31", "2020-01-15", "2020-01-31", "2020-01-15",
      "2020-01-31", "2020-01-08", "2020-01-15"
    )),
    time_in_cohort = 0
  )
}

run_state_id_fixture <- function(data, type, batch_size, labels = c("B", "A", "Unused"),
                                 gap = "None") {
  study_path <- tempfile("c2t-state-ids-")
  dir.create(study_path)
  on.exit(unlink(study_path, recursive = TRUE), add = TRUE)
  study <- cohort2TrajectoryConfiguration(
    studyName = "ids", pathToStudy = study_path, useCDM = FALSE,
    atlasTargetCohort = "0", stateCohortLabels = labels,
    stateCohortPriorityOrder = c("B", "A", "Unused"),
    stateSelectionType = "Priority", trajectoryType = type,
    lengthOfStay = 7, batchSize = batch_size, oocFix = gap,
    allowedStatesList = createStateList(c("A", "B", "Unused")),
    runSavedStudy = TRUE
  )
  utils::write.csv(data, file.path(study_path, "ids", "Data", "importedDataCleaned_0.csv"),
                   row.names = FALSE)
  result <- createTrajectories(studyEnv = study)
  filename <- if (type == "Discrete") "patientDataDiscrete.csv" else "patientDataContinuous.csv"
  saved <- utils::read.csv(file.path(study_path, "ids", "Data", filename))
  expect_equal(as.character(saved$state_id), as.character(result$state_id))
  result <- as.data.frame(result)
  result <- result[order(result$subject_id, result$time_in_cohort, result$state_label), ]
  rownames(result) <- NULL
  result
}

test_that("state IDs and histories are independent of batch size and row order", {
  data <- make_state_id_fixture()
  for (type in c("Discrete", "Continuous")) {
    whole <- run_state_id_fixture(data, type, 100)
    expect_equal(run_state_id_fixture(data, type, 1), whole)
    expect_equal(run_state_id_fixture(data, type, 2), whole)
    expect_equal(run_state_id_fixture(data[rev(seq_len(nrow(data))), ], type, 1), whole)
    mapping <- unique(whole[c("state_label", "state_id")])
    expect_equal(nrow(mapping), length(unique(mapping$state_label)))
    expect_equal(nrow(mapping), length(unique(mapping$state_id)))
    expect_equal(as.character(whole$state_id[whole$state_label == "A"]),
                 rep("3", sum(whole$state_label == "A")))
    expect_equal(as.character(whole$state_id[whole$state_label == "B"]),
                 rep("2", sum(whole$state_label == "B")))
    # An absent configured state still reserves its ID.
    expect_false("4" %in% as.character(whole$state_id))
    exit_id <- if (type == "Discrete") "6" else "5"
    expect_true(all(as.character(whole$state_id[whole$state_label == "EXIT"]) == exit_id))
  }
})

test_that("unconfigured labels and gap states receive study-wide IDs", {
  data <- make_state_id_fixture()
  # Create an empty weekly cycle between B and A for the third patient.
  data$cohort_start_date[7] <- as.Date("2020-01-16")
  data$cohort_end_date[7] <- as.Date("2020-01-22")
  for (gap in c("None", "Background")) {
    whole <- run_state_id_fixture(data, "Discrete", 100, labels = "Unused", gap = gap)
    expect_equal(run_state_id_fixture(data, "Discrete", 1, labels = "Unused", gap = gap), whole)
    gap_label <- if (gap == "None") "OUT OF COHORT" else gap
    expect_true(gap_label %in% whole$state_label)
    mapping <- unique(whole[c("state_label", "state_id")])
    expect_equal(nrow(mapping), length(unique(mapping$state_id)))
  }
})
