episode_import_fixture <- function() {
  origin <- as.Date("2020-01-01")
  data.frame(
    subject_id = c("p", "p", "p", "p", "p", "p", "p", "q"),
    cohort_definition_id = c("0", "A", "0", "B", "0", "Gap only", "0", "0"),
    cohort_start_date = origin + c(20, -2, 0, 24, 40, 12, 0, 0),
    cohort_end_date = origin + c(29, 25, 9, 29, 44, 17, 9, 4)
  )
}

test_that("all-target import clips states to stable episodes and preserves state-free periods", {
  input <- episode_import_fixture()
  cleaned <- .cleanAllTargetEpisodes(input, NULL)
  origin <- as.Date("2020-01-01")
  expect_equal(cleaned$subject_id, c(rep("p", 6), "q"))
  expect_equal(cleaned$episode_id, c(1L, 1L, 2L, 2L, 2L, 3L, 1L))
  expect_equal(cleaned$cohort_definition_id, c("0", "A", "0", "A", "B", "0", "0"))
  expect_equal(cleaned$cohort_start_date, origin + c(0, 0, 20, 20, 24, 40, 0))
  expect_equal(cleaned$cohort_end_date, origin + c(9, 9, 29, 25, 29, 44, 4))
  expect_identical(.cleanAllTargetEpisodes(input[nrow(input):1L, ], NULL), cleaned)

  required <- .cleanAllTargetEpisodes(input, c("A", "B"))
  expect_equal(unique(required$episode_id), 2L)
  expect_equal(required$cohort_definition_id, c("0", "A", "B"))
  expect_equal(nrow(.cleanAllTargetEpisodes(input, c("A", "Gap only"))), 0L)
  expect_equal(nrow(.cleanAllTargetEpisodes(input[FALSE, ], NULL)), 0L)
  expect_equal(nrow(.cleanAllTargetEpisodes(input[input$cohort_definition_id != "0", ], NULL)), 0L)
})

test_that("all-target import rejects shared dates but accepts adjacent episodes", {
  origin <- as.Date("2020-01-01")
  input <- data.frame(
    subject_id = "p", cohort_definition_id = "0",
    cohort_start_date = origin + c(0, 9), cohort_end_date = origin + c(9, 19)
  )
  expect_error(.cleanAllTargetEpisodes(input, NULL), "Target intervals overlap")
  input$cohort_start_date[2] <- origin + 10
  expect_equal(.cleanAllTargetEpisodes(input, NULL)$episode_id, c(1L, 2L))
})

test_that("raw episode columns do not alter First-target cleaning or assigned episode IDs", {
  input <- episode_import_fixture()
  expectedFirst <- cleanCohortData(input, NULL)
  expectedAll <- .cleanAllTargetEpisodes(input, NULL)
  input$episode_id <- rev(seq_len(nrow(input)))
  expect_identical(cleanCohortData(input, NULL), expectedFirst)
  expect_identical(.cleanAllTargetEpisodes(input, NULL), expectedAll)
})

test_that("temporary IDs distinguish subject and episode and restore both fields", {
  input <- data.frame(
    subject_id = c("a:1", "a", "a:1", "a", "9007199254740993", "001"),
    episode_id = c(1L, 11L, 2L, 1L, 1L, 1L)
  )
  encoded <- .encodeSubjectIds(input)
  expect_equal(length(unique(encoded$data$subject_id)), nrow(input))
  retained <- c(6L, 1L, 4L, 5L)
  native <- encoded$data[retained, "subject_id", drop = FALSE]
  restored <- .restoreSubjectIds(native, encoded$originals)
  expect_equal(restored$subject_id, input$subject_id[retained])
  expect_equal(restored$episode_id, input$episode_id[retained])
  for (invalid in list(NA_real_, 0, -1, 1.5, Inf, .Machine$integer.max + 1, "1")) {
    input$episode_id <- rep(invalid, nrow(input))
    expect_error(.encodeSubjectIds(input), "episode_id must contain positive whole numbers")
  }
})

test_that("state composition and bounding keep adjacent episodes separate", {
  origin <- as.Date("2020-01-01")
  input <- data.frame(
    subject_id = "p", cohort_definition_id = c("0", "0", "A"),
    cohort_start_date = origin + c(0, 10, 0), cohort_end_date = origin + c(9, 19, 19)
  )
  cleaned <- .cleanAllTargetEpisodes(input, NULL)
  bounded <- .boundTrajectoryData(cleaned)
  expect_equal(sum(bounded$cohort_definition_id == "0"), 2L)
  expect_equal(bounded$episode_id, cleaned$episode_id)
  composed <- .composeStateIntervals(cleaned, "A", list())$data
  states <- composed[composed$cohort_definition_id != "0", ]
  expect_equal(states$episode_id, c(1L, 2L))
  expect_equal(states$cohort_start_date, origin + c(0, 10))
  expect_equal(states$cohort_end_date, origin + c(9, 19))
  expect_equal(states$time_in_cohort, c(0, 0))
})

test_that("continuous priority starts transition history anew in each episode", {
  origin <- as.Date("2020-01-01")
  input <- data.frame(
    subject_id = "p", episode_id = rep(c(1L, 2L), each = 3L),
    cohort_definition_id = c("START", "A", "EXIT", "START", "B", "EXIT"),
    cohort_start_date = origin + c(0, 0, 10, 10, 10, 20),
    cohort_end_date = origin + c(0, 9, 10, 10, 19, 20)
  )
  selected <- .selectContinuousPriorityIntervals(input, c("A", "B"),
                                                  list(A = character(0), B = character(0)), "A")
  clinical <- selected[!selected$cohort_definition_id %in% c("START", "EXIT"), ]
  expect_equal(clinical$cohort_definition_id, c("A", "B"))
  expect_equal(clinical$episode_id, c(1L, 2L))
})
