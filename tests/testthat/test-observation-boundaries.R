boundary_fixture <- function() {
  data.frame(
    subject_id = c(1L, 1L, 2L), cohort_definition_id = c("0", "A", "0"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 7, 0),
    cohort_end_date = as.Date("2020-01-01") + c(20, 13, 9),
    time_in_cohort = 0
  )
}

boundary_trajectory <- function(data, type, outside = FALSE, absorbing = NULL) {
  if (type == "Discrete") {
    getTrajectoriesDiscrete(NULL, data, stateDuration = 7, statePriorityVector = "A",
                           addPersonalData = FALSE, outOfCohortAllowed = outside,
                           absorbingStates = absorbing)
  } else {
    getTrajectoriesContinuous(NULL, data, stateSelection = 3, statePriorityVector = "A",
                             addPersonalData = FALSE, outOfCohortAllowed = outside,
                             absorbingStates = absorbing)
  }
}

test_that("target boundaries survive sparse and absent state records", {
  for (type in c("Discrete", "Continuous")) {
    result <- boundary_trajectory(boundary_fixture(), type)
    expect_equal(result$subject_id[result$state_label == "START"], c(1, 2))
    expect_equal(result$state_start_date[result$state_label == "START"],
                 rep(as.Date("2020-01-01"), 2))
    expect_equal(result$state_start_date[result$state_label == "EXIT"],
                 as.Date(c("2020-01-22", "2020-01-11")))
    expect_equal(result$state_end_date[result$state_label == "A"], as.Date("2020-01-14"))
    if (type == "Discrete") {
      expect_equal(result$state_label[result$subject_id == 1],
                   c("START", "OUT OF COHORT", "A", "OUT OF COHORT", "EXIT"))
    } else {
      expect_equal(result$state_label[result$subject_id == 2], c("START", "EXIT"))
    }
  }
})

test_that("the existing outside-cohort option controls the ending boundary", {
  data <- boundary_fixture()[1:2, ]
  data$cohort_start_date[2] <- as.Date("2019-12-30")
  data$cohort_end_date[2] <- as.Date("2020-01-28")
  for (type in c("Discrete", "Continuous")) {
    for (outside in c(FALSE, TRUE)) {
      result <- boundary_trajectory(data, type, outside)
      expect_equal(min(result$state_start_date), as.Date("2020-01-01"))
      expect_equal(result$state_start_date[result$state_label == "EXIT"],
                   as.Date(if (outside) "2020-01-29" else "2020-01-22"))
      expect_equal(result$time_in_cohort,
                   as.numeric(result$state_start_date - as.Date("2020-01-01")) / 365.25)
    }
  }
})

test_that("absorbing states still end trajectories before the target endpoint", {
  for (type in c("Discrete", "Continuous")) {
    result <- boundary_trajectory(boundary_fixture()[1:2, ], type, absorbing = "A")
    expect_equal(result$state_start_date[result$state_label == "EXIT"], as.Date("2020-01-15"))
    expect_equal(result$time_in_cohort[result$state_label == "EXIT"], 14 / 365.25)
  }
})

test_that("the public workflow accepts a target-only batch", {
  path <- tempfile("c2t-boundaries-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  data <- boundary_fixture()[3, ]
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = type, useCDM = FALSE,
      atlasTargetCohort = "0", stateCohortLabels = "A", stateCohortPriorityOrder = "A",
      stateSelectionType = "Priority", trajectoryType = type, lengthOfStay = 7
    )
    utils::write.csv(data, file.path(path, type, "Data", "importedDataCleaned_0.csv"), row.names = FALSE)
    result <- createTrajectories(studyEnv = study)
    expect_equal(result$state_start_date[result$state_label == "EXIT"], as.Date("2020-01-11"))
  }
})
