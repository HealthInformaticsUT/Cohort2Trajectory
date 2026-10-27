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

test_that("native transition matching follows R case conversion for accented labels", {
  label <- "\u00c4ge faas"
  skip_if(tolower(label) != "\u00e4ge faas", "R locale does not support this case conversion")
  data <- data.frame(
    subject_id = 1L, cohort_definition_id = c("0", "A", label, label),
    cohort_start_date = as.Date("2020-01-01") + c(0, 0, 7, 14),
    cohort_end_date = as.Date("2020-01-01") + c(20, 6, 13, 20)
  )
  rules <- setNames(list(tolower(label), toupper(label)), c("A", label))
  for (method in 1:3) {
    discrete <- getTrajectoriesDiscrete(
      NULL, data, stateDuration = 7, stateSelection = method,
      statePriorityVector = c("A", label), allowedStatesList = rules, addPersonalData = FALSE
    )
    continuous <- getTrajectoriesContinuous(
      NULL, data, stateSelection = method, statePriorityVector = c("A", label),
      allowedStatesList = rules, addPersonalData = FALSE
    )
    expect_identical(discrete$state_label, c("START", "A", label, label, "EXIT"))
    expect_identical(continuous$state_label, c("START", "A", label, label, "EXIT"))
  }
  # An unrecorded named fallback must use the same comparison rules.
  gap <- data[c(1, 2), ]
  fallback <- getTrajectoriesDiscrete(
    NULL, gap, stateDuration = 7, stateSelection = 3, statePriorityVector = "A",
    oocFix = label, allowedStatesList = rules, addPersonalData = FALSE
  )
  expect_identical(fallback$state_label, c("START", "A", label, label, "EXIT"))
  # Preserve explicit input encodings before native comparison forms are made.
  encoded <- iconv(label, from = "UTF-8", to = "latin1")
  gap$time_in_cohort <- 0
  encodedRules <- setNames(list(tolower(encoded), toupper(encoded)), c("A", encoded))
  native <- getDiscreteStates(3, encoded, 7, 1L, gap, c("A", encoded), encodedRules)
  expect_identical(lapply(native$state_label, charToRaw),
                    lapply(c("A", encoded, encoded), charToRaw))
  expect_error(getDiscreteStates(3, c("None", encoded), 7, 1L, gap, "A", list()),
                "single string")
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

test_that("continuous absorbing cutoff requires the same patient's EXIT", {
  data <- data.frame(
    subject_id = c(1L, 1L, 2L, 2L, 1L),
    state_label = c("START", "Death", "START", "EXIT", "EXIT"),
    state_start_date = as.Date("2020-01-01") + c(0, 1, 0, 4, 4),
    state_end_date = as.Date("2020-01-01") + c(0, 1, 0, 4, 4),
    state_id = c(1, 2, 1, 99, 3), time_in_cohort = c(0, 1, 0, 4, 4) / 365.25
  )
  result <- removeAfterAbsorbingStatesContinuous(data, c(1L, 2L), "Death")
  exits <- result[result$state_label == "EXIT", ]
  expect_equal(exits$state_id, c(3, 99))
  expect_equal(exits$state_start_date, as.Date(c("2020-01-03", "2020-01-05")))
  # Another patient's EXIT must not conceal a missing boundary.
  expect_error(removeAfterAbsorbingStatesContinuous(data[1:4, ], c(1L, 2L), "Death"),
               "Missing EXIT after absorbing state for internal patient ID 1")
  # Nor may the search run beyond the end when no EXIT remains at all.
  expect_error(removeAfterAbsorbingStatesContinuous(data[1:2, ], 1L, "Death"),
               "Missing EXIT after absorbing state for internal patient ID 1")
})
