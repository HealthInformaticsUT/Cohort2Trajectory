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

test_that("continuous priority selects allowed states before removing overlaps", {
  origin <- as.Date("2020-01-01")
  input <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "C", "A", "B"),
    cohort_start_date = origin + c(0, 0, 2, 2),
    cohort_end_date = origin + c(6, 1, 4, 6)
  )
  run <- function(rules, absorbing = NULL) {
    result <- getTrajectoriesContinuous(
      NULL, input, stateSelection = 3, statePriorityVector = c("A", "B", "C"),
      allowedStatesList = rules, absorbingStates = absorbing, addPersonalData = FALSE
    )
    data.frame(label = result$state_label,
               start = as.numeric(result$state_start_date - origin),
               end = as.numeric(result$state_end_date - origin))
  }
  # A cannot erase B's first three days. B needs no self-transition just
  # because the rejected A record ends during its uninterrupted interval.
  for (absorbing in list(NULL, "A")) {
    expect_equal(run(list(C = "b", B = character(0)), absorbing),
                  data.frame(label = c("START", "C", "B", "EXIT"),
                             start = c(0, 0, 2, 7), end = c(0, 1, 6, 7)))
  }
  # B is prohibited from C, but becomes eligible after the permitted A ends.
  expect_equal(run(list(C = "A", A = "B")),
                data.frame(label = c("START", "C", "A", "B", "EXIT"),
                           start = c(0, 0, 2, 5, 7), end = c(0, 1, 4, 6, 7)))
  # A selected absorbing interval needs no outgoing rule and keeps its endpoint.
  expect_equal(run(list(C = "A"), "A"),
                data.frame(label = c("START", "C", "A", "EXIT"),
                           start = c(0, 0, 2, 5), end = c(0, 1, 4, 5)))
  # Later higher-priority records cannot shorten selected absorbing B.
  # Ending one history must not stop or carry rules into the next patient.
  input$cohort_start_date[input$cohort_definition_id == "A"] <- origin + 3
  input <- rbind(input, transform(input, subject_id = 2L))
  expect_equal(run(list(C = "B"), "B"),
                data.frame(label = rep(c("START", "C", "B", "EXIT"), 2),
                           start = rep(c(0, 0, 2, 7), 2), end = rep(c(0, 1, 6, 7), 2)))
})
