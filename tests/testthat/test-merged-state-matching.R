test_that("merged-state rules match whole labels literally", {
  path <- tempfile("c2t-merged-labels-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  for (death in c("Death", "Death [")) {
    risk <- paste0(death, " risk")
    labels <- c(death, risk, "A")
    rules <- createStateList(labels)
    rules[[death]] <- character(0)
    input <- data.frame(
      subject_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L),
      cohort_definition_id = c("0", risk, "A", "A", "0", death, "A"),
      cohort_start_date = as.Date("2020-01-01") + c(0, 0, 0, 14, 0, 7, 7),
      cohort_end_date = as.Date("2020-01-01") + c(20, 6, 6, 20, 20, 7, 7)
    )
    study <- cohort2TrajectoryConfiguration(
      pathToStudy = path, studyName = if (death == "Death") "similar" else "literal",
      useCDM = FALSE, trajectoryDataObject = input, trajectoryType = "Continuous",
      stateCohortLabels = labels, stateCohortPriorityOrder = labels,
      stateSelectionType = "Priority", stateCohortAbsorbing = death,
      allowedStatesList = rules, mergeStates = TRUE
    )
    expect_warning(getDataForStudy(studyEnv = study), NA)
    expect_false(risk %in% study$stateCohortAbsorbing)
    expect_false(risk %in% study$allowedStatesList[[death]])
    result <- createTrajectories(studyEnv = study)
    # Risk records must not terminate follow-up. Actual death still does.
    expect_equal(result$state_start_date[result$state_label == "EXIT"],
                 as.Date(c("2020-01-22", "2020-01-09")))
  }
})
