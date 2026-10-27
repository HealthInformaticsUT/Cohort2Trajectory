test_that("invalid batch sizes fail before creating files or reading patient data", {
  path <- tempfile("c2t-batch-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  # This recycled vector previously excluded every second patient silently.
  expect_error(cohort2TrajectoryConfiguration(
    pathToStudy = path, useCDM = FALSE, batchSize = c(1, NA)
  ), "batchSize must be a single positive whole number")
  expect_length(list.files(path), 0)

  # Explicit generation overrides must be checked too, for either format.
  for (type in 0:1) {
    for (size in list(c(1, NA), NA_real_, 0, -1, 1.5, Inf, NULL)) {
      expect_error(createTrajectories(
        trajectoryType = type, lengthOfStay = 7, batchSize = size
      ), "batchSize must be a single positive whole number")
    }
  }
})
