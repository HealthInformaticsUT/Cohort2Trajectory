test_that("date overlap counts the shared days including both endpoints", {
  origin <- as.Date("2020-01-01")
  intervals <- data.frame(
    start = c(0, 7, 7, 7, 7, 7, 7),
    end = c(6, 13, 13, 13, 13, 13, 13),
    other_start = c(7, 0, 0, 8, 7, 10, 13),
    other_end = c(13, 7, 20, 9, 13, 20, 13),
    expected = c(0, 1, 7, 2, 7, 4, 1)
  )
  actual <- mapply(function(start, end, other_start, other_end) {
    daysOverlap(origin + start, origin + end,
                origin + other_start, origin + other_end)
  }, intervals$start, intervals$end, intervals$other_start, intervals$other_end)
  expect_equal(unname(actual), intervals$expected)
})

test_that("discrete selection uses actual overlap at boundaries and within long records", {
  data <- data.frame(
    subject_id = c(1L, 1L, 2L, 2L),
    cohort_definition_id = c("A", "B", "A", "B"),
    cohort_start_date = as.Date("2020-01-01") + c(0, 8, 0, 7),
    cohort_end_date = as.Date("2020-01-01") + c(7, 13, 20, 13),
    time_in_cohort = c(0, 8, 0, 7) / 365.25
  )
  # A still overlaps the second week on its first day.
  boundary <- getDiscreteStates(3L, "None", 7L, 1L, data,
                                 c("A", "B"), list())
  expect_equal(boundary$state_label, c("A", "A"))
  # Both records cover all seven days of week two, so priority breaks the tie.
  contained <- getDiscreteStates(2L, "None", 7L, 2L, data,
                                  c("B", "A"), list())
  expect_equal(contained$state_label, c("A", "B", "A"))
})
