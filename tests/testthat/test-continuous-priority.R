test_that("continuous priority removes all shared days without invalid fragments", {
  # B surrounds A, ends with A, crosses either edge, or lies entirely inside A.
  cases <- list(
    list(b = cbind(0, 9), expected = data.frame(label = c("B", "A", "B"), start = c(0, 3, 6), end = c(2, 5, 9))),
    list(b = cbind(0, 5), expected = data.frame(label = c("B", "A"), start = c(0, 3), end = c(2, 5))),
    list(b = cbind(c(0, 5), c(3, 9)), expected = data.frame(label = c("B", "A", "B"), start = c(0, 3, 6), end = c(2, 5, 9))),
    list(b = cbind(4, 4), expected = data.frame(label = "A", start = 3, end = 5))
  )
  origin <- as.Date("2020-01-01")
  for (case in cases) {
    data <- data.frame(
      subject_id = 1L, cohort_definition_id = c("0", "A", rep("B", nrow(case$b))),
      cohort_start_date = origin + c(0, 3, case$b[, 1]),
      cohort_end_date = origin + c(9, 5, case$b[, 2]), time_in_cohort = 0
    )
    result <- getTrajectoriesContinuous(
      NULL, data, stateSelection = 3, statePriorityVector = c("A", "B"),
      addPersonalData = FALSE
    )
    states <- result[!result$state_label %in% c("START", "EXIT"), ]
    actual <- data.frame(label = states$state_label,
                         start = as.numeric(states$state_start_date - origin),
                         end = as.numeric(states$state_end_date - origin))
    expect_equal(actual, case$expected)
  }
})

test_that("continuous priority cannot silently discard unlisted states", {
  data <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", "B"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 3),
    cohort_end_date = as.Date("2020-01-01") + c(6, 2, 6),
    time_in_cohort = 0
  )
  run <- function(priority, input = data) {
    getTrajectoriesContinuous(
      NULL, input, stateSelection = 3, statePriorityVector = priority,
      addPersonalData = FALSE
    )
  }
  expect_error(run("A"), "States missing from statePriorityVector: B", fixed = TRUE)
  expect_error(run(NULL), "States missing from statePriorityVector: A, B", fixed = TRUE)
  # No priority is needed when the patient has only a target interval.
  expect_equal(run(NULL, data[1, ])$state_label, c("START", "EXIT"))
})
