test_that("final cycles and EXIT preserve the supplied history endpoint", {
  data <- data.frame(
    subject_id = c(1L, 2L), cohort_definition_id = "A",
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date(c("2020-01-10", "2020-01-14")),
    time_in_cohort = 0
  )
  result <- getTrajectoriesDiscrete(
    cdm = NULL, cohortData = data, stateDuration = 7,
    statePriorityVector = "A", addPersonalData = FALSE
  )
  cycles <- result[result$state_label == "A", ]
  exit <- result[result$state_label == "EXIT", ]
  # Patient 1 has a partial final cycle. Patient 2 ends on a full-cycle boundary.
  expect_equal(cycles$state_end_date,
               as.Date(c("2020-01-07", "2020-01-10", "2020-01-07", "2020-01-14")))
  expect_equal(exit$state_start_date, as.Date(c("2020-01-11", "2020-01-15")))
  expect_equal(exit$time_in_cohort, c(10, 14) / 365.25)
})
