continuous_rule_fixture <- function(rules = list(), absorbing = NULL) {
  data <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", "B", "C"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 13, 20),
    time_in_cohort = 0
  )
  getTrajectoriesContinuous(
    NULL, data, stateSelection = 3, statePriorityVector = c("A", "B", "C"),
    addPersonalData = FALSE, allowedStatesList = rules, absorbingStates = absorbing
  )
}

test_that("empty continuous transition rules are unrestricted", {
  result <- continuous_rule_fixture()
  expect_equal(result$state_label, c("START", "A", "B", "C", "EXIT"))
})

test_that("a prohibited absorbing state does not truncate later valid records", {
  result <- continuous_rule_fixture(list(A = "c", B = "B", C = "C"), absorbing = "B")
  expect_equal(result$state_label, c("START", "A", "C", "EXIT"))
  expect_equal(result$state_start_date[result$state_label == "EXIT"], as.Date("2020-01-22"))
  # A permitted absorbing state ends the history, with no rules needed after it.
  permitted <- continuous_rule_fixture(list(A = "B"), absorbing = "B")
  expect_equal(permitted$state_label, c("START", "A", "B", "EXIT"))
  expect_equal(permitted$state_start_date[permitted$state_label == "EXIT"], as.Date("2020-01-15"))
})

test_that("continuous filtering counts retained repeats and diagnoses missing rules", {
  data <- data.frame(
    subject_id = 1L, state_label = c("START", "A", "B", "A", "EXIT"),
    state_start_date = as.Date("2020-01-01") + 0:4,
    state_end_date = as.Date("2020-01-01") + 0:4,
    state_id = c(1, 2, 3, 2, 4), time_in_cohort = (0:4) / 365.25,
    seq_ordinal = 1
  )
  result <- removeProhibitedTransitionsContinuous(data, 1L, list(A = "A", B = "B"))
  expect_equal(result$state_label, c("START", "A", "A", "EXIT"))
  expect_equal(result$seq_ordinal, c(1, 1, 2, 1))
  expect_error(continuous_rule_fixture(list(B = "B")),
               "No allowed-transition entry for state 'A'")
})
