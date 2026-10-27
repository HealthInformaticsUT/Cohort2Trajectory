transition_fixture <- function(labels, starts, ends, subjects = rep(1L, length(labels))) {
  data.frame(subject_id = subjects, cohort_definition_id = labels,
             cohort_start_date = as.Date("2020-01-01") + starts,
             cohort_end_date = as.Date("2020-01-01") + ends,
             time_in_cohort = starts / 365.25)
}

discrete_labels <- function(data, method, rules, priority = c("A", "B", "C", "D"),
                            gap = "None") {
  getDiscreteStates(method, gap, 7L, unique(data$subject_id), data, priority, rules)$state_label
}

test_that("all methods prevent forbidden transitions and retain patient-specific memory", {
  data <- transition_fixture(c("A", "B", "C", "B"), c(0, 7, 14, 0), c(6, 13, 20, 6),
                             c(1L, 1L, 1L, 2L))
  rules <- list(A = c("A", "C"), B = "B", C = "C", D = "D")
  for (method in 1:3) {
    expect_equal(discrete_labels(data, method, rules), c("A", "OUT OF COHORT", "C", "B"))
  }
  expect_equal(discrete_labels(data, 3, rules, gap = "Last present state"),
               c("A", "A", "C", "B"))
})

test_that("a forbidden best candidate does not hide a permitted alternative", {
  data <- transition_fixture(c("A", "B", "C", "D"), c(0, 7, 8, 14), c(6, 13, 12, 20))
  rules <- list(A = "C", B = "B", C = "D", D = "D")
  for (method in 1:3) {
    expect_equal(discrete_labels(data, method, rules), c("A", "C", "D"))
  }
})

test_that("selection retains First, Overlap and Priority ranking among legal candidates", {
  data <- transition_fixture(c("A", "B", "C"), c(0, 7, 8), c(6, 9, 13))
  rules <- createStateList(c("A", "B", "C"))
  expect_equal(discrete_labels(data, 1, rules, c("A", "C", "B")), c("A", "B"))
  expect_equal(discrete_labels(data, 2, rules, c("A", "C", "B")), c("A", "C"))
  expect_equal(discrete_labels(data, 3, rules, c("A", "C", "B")), c("A", "C"))
})

test_that("First only selects records overlapping the current cycle", {
  data <- transition_fixture(c("A", "B"), c(0, 14), c(13, 20))
  expect_equal(discrete_labels(data, 1, createStateList(c("A", "B"))), c("A", "A", "B"))
})

test_that("gaps cannot reset restrictions and named fallback states respect them", {
  data <- transition_fixture(c("A", "B"), c(0, 14), c(6, 20))
  rules <- list(A = "A", B = "B", C = "B")
  expect_equal(discrete_labels(data, 3, rules), c("A", "OUT OF COHORT", "OUT OF COHORT"))
  expect_equal(discrete_labels(data, 3, rules, gap = "C"),
               c("A", "OUT OF COHORT", "OUT OF COHORT"))
  rules$A <- c("A", "C")
  expect_equal(discrete_labels(data, 3, rules, gap = "C"), c("A", "C", "B"))
})

test_that("empty destinations and forbidden self-transitions yield unresolved cycles", {
  data <- transition_fixture("A", 0, 13)
  expect_equal(discrete_labels(data, 3, list(A = character()), gap = "Last present state"),
               c("A", "OUT OF COHORT"))
})

test_that("unrestricted and case-insensitive destination lists remain supported", {
  data <- transition_fixture(c("A", "B", "C"), c(0, 7, 14), c(6, 13, 20))
  expect_equal(discrete_labels(data, 3, list()), c("A", "B", "C"))
  expect_equal(discrete_labels(data, 3, list(A = "b", B = "c", C = "c")), c("A", "B", "C"))
  expect_error(discrete_labels(data, 3, list(B = "B")), "No allowed-transition entry for state 'A'")
})

test_that("selected absorbing states stop discrete cycles without outgoing rules", {
  data <- transition_fixture(c("0", "A", "B", "C", "0", "C"),
                              c(0, 0, 7, 14, 0, 0), c(20, 6, 13, 20, 13, 13),
                              c(1L, 1L, 1L, 1L, 2L, 2L))
  run <- function(method, rules) {
    getTrajectoriesDiscrete(NULL, data, stateDuration = 7,
                            stateSelection = method, statePriorityVector = c("A", "B", "C"),
                            absorbingStates = "B", allowedStatesList = rules,
                            addPersonalData = FALSE)
  }
  for (method in 1:3) {
    # No B entry is needed after absorption. The next patient still runs.
    permitted <- run(method, list(A = "B", C = "C"))
    expect_equal(permitted$state_label, c("START", "A", "B", "EXIT", "START", "C", "C", "EXIT"))
    expect_equal(permitted$state_start_date[permitted$state_label == "EXIT"],
                  as.Date(c("2020-01-15", "2020-01-15")))
    # A recorded but prohibited absorbing state must not end follow-up.
    prohibited <- run(method, list(A = "C", C = "C"))
    expect_equal(prohibited$state_label,
                  c("START", "A", "OUT OF COHORT", "C", "EXIT", "START", "C", "C", "EXIT"))
    expect_equal(prohibited$state_start_date[prohibited$state_label == "EXIT"],
                  as.Date(c("2020-01-22", "2020-01-15")))
  }
})

test_that("missing priority entries cannot silently turn records into gaps", {
  data <- transition_fixture(c("0", "A", "B", "Outside"), c(0, 0, 7, 21), c(13, 6, 13, 27))
  run <- function(method, priority, input = data) {
    getTrajectoriesDiscrete(NULL, input, stateDuration = 7,
                            stateSelection = method, statePriorityVector = priority,
                            addPersonalData = FALSE)
  }
  for (method in 1:3) {
    expect_error(run(method, "A"), "State 'B' is missing from statePriorityVector", fixed = TRUE)
    # Records outside the target interval do not require a priority entry.
    expect_identical(run(method, c("A", "B"))$state_label, c("START", "A", "B", "EXIT"))
  }
  expect_error(run(1, NULL), "State 'A' is missing from statePriorityVector", fixed = TRUE)
  expect_identical(run(1, NULL, data[1, ])$state_label,
                    c("START", "OUT OF COHORT", "OUT OF COHORT", "EXIT"))
})

test_that("discrete routines reject requested patients without input rows", {
  input <- transition_fixture("A", 0, 6)
  generate <- function(data, ids) getDiscreteStates(3, "None", 7L, ids, data, "A", list())
  cycles <- generate(input, 1L)
  # Check both an empty output buffer and a missing patient after a valid one.
  for (ids in list(c(2L, 1L), c(1L, 2L))) {
    expect_error(generate(input, ids), "No input rows for requested internal patient ID 2")
    expect_error(removeAfterAbsorbingStatesDiscrete(cycles, ids, character(0)),
                 "No input rows for requested internal patient ID 2")
  }
  expect_error(generate(input[0, ], 1L), "No input rows for requested internal patient ID 1")
  expect_error(removeAfterAbsorbingStatesDiscrete(cycles[0, ], 1L, character(0)),
               "No input rows for requested internal patient ID 1")
  # No requested patients is a valid empty operation, not a mismatch.
  expect_equal(nrow(generate(input[0, ], integer(0))), 0L)
  expect_equal(nrow(removeAfterAbsorbingStatesDiscrete(cycles[0, ], integer(0), character(0))), 0L)
})
