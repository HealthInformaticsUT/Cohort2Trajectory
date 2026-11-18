test_that("compact printing preserves study settings and complete trajectory data", {
  path <- tempfile("c2t-print-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  input <- data.frame(
    subject_id = c("synthetic_001", "synthetic_001", "synthetic_002"),
    cohort_definition_id = c("0", "A", "0"),
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-01") + c(13, 6, 13)
  )
  for (type in c("Discrete", "Continuous")) {
    study <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = path, useCDM = FALSE,
      trajectoryDataObject = input, stateCohortLabels = c("A", "Absent"),
      stateCohortPriorityOrder = c("A", "Absent"),
      stateSelectionType = "Priority", trajectoryType = type, cycleLength = 7
    )
    expect_true(is.environment(study))
    expect_s3_class(study, "c2t_study")
    settings <- as.list(study, all.names = TRUE)
    studyText <- capture.output(visibleStudy <- withVisible(print(study)))
    expect_false(visibleStudy$visible)
    expect_identical(visibleStudy$value, study)
    expect_identical(as.list(study, all.names = TRUE), settings)
    expect_match(paste(studyText, collapse = "\n"), "Clinical states configured: 2")
    expect_false(any(grepl("synthetic_", studyText)))

    getDataForStudy(studyEnv = study)
    result <- createTrajectories(studyEnv = study)
    expect_true(is.data.frame(result))
    expect_s3_class(result, "c2t_trajectories")
    before <- serialize(result, NULL)
    ordinary <- as.data.frame(result)
    expect_identical(names(result), names(ordinary))
    expect_identical(lapply(result, identity), lapply(ordinary, identity))
    expect_identical(result$subject_id, ordinary$subject_id)
    expect_identical(as.data.frame(result[result$state_label == "A", ]),
                     ordinary[ordinary$state_label == "A", ])

    files <- c(file.path(path, type, "Data", paste0("patientData", type, ".csv")),
               file.path(path, type, "Settings", "analysisSettings.rds"))
    checksums <- tools::md5sum(files)
    text <- capture.output(visible <- withVisible(print(result, n = 2)))
    expect_false(visible$visible)
    expect_identical(visible$value, result)
    expect_match(paste(text, collapse = "\n"),
                 paste0("Patients: 2 \\| Clinical states present: 1 \\| Rows: ",
                        if (type == "Discrete") 8 else 5))
    expect_match(paste(text, collapse = "\n"), "Boundary rows \\(START/EXIT\\): 4")
    expect_match(paste(text, collapse = "\n"),
                 paste0("Unresolved rows \\(OUT OF COHORT\\): ",
                        if (type == "Discrete") 3 else 0))
    expect_match(paste(text, collapse = "\n"), "Preview: 2 of")
    expect_true(any(grepl("synthetic_001", text)))
    expect_identical(serialize(result, NULL), before)
    expect_identical(tools::md5sum(files), checksums)
    expectedCsv <- file.path(path, "ordinary.csv")
    save_object(ordinary, expectedCsv)
    expect_identical(unname(tools::md5sum(files[1])),
                     unname(tools::md5sum(expectedCsv)))

    mapping <- result[c("state_id", "state_label")]
    expect_identical(capture.output(print(mapping)),
                     capture.output(print(as.data.frame(mapping))))
    empty <- capture.output(print(result[FALSE, ]))
    expect_match(paste(empty, collapse = "\n"),
                 "Patients: 0 \\| Clinical states present: 0 \\| Rows: 0")

    restored <- cohort2TrajectoryConfiguration(
      studyName = type, pathToStudy = path, useCDM = FALSE,
      trajectoryDataObject = input, runSavedStudy = TRUE
    )
    expect_true(is.environment(restored))
    expect_s3_class(restored, "c2t_study")
    capture.output(print(restored))
    expect_identical(tools::md5sum(files), checksums)
  }
})
