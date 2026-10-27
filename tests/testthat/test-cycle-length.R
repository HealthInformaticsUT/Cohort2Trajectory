test_that("invalid cycle lengths fail before configuration creates files", {
  path <- tempfile("c2t-cycle-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  for (duration in list(0, -1, 2.5, NA_real_, Inf, c(1, 7))) {
    expect_error(cohort2TrajectoryConfiguration(
      pathToStudy = path, useCDM = FALSE, lengthOfStay = duration
    ), "lengthOfStay must be a single positive whole number")
  }
  expect_length(list.files(path), 0)
})

test_that("generation rejects missing or invalid lengths before reading data", {
  # No study or input is needed to diagnose these invalid settings.
  expect_error(createTrajectories(trajectoryType = "Discrete", lengthOfStay = NULL),
               "lengthOfStay must be a single positive whole number")
  expect_error(getTrajectoriesDiscrete(NULL, NULL, stateDuration = 2.5),
               "stateDuration must be a single positive whole number")
  # Guard the native entry point too, before it accesses any patient data.
  expect_error(getDiscreteStates(1, "None", 0L, integer(), data.frame(), character(), list()),
               "stateDuration must be a positive number of days")
})
