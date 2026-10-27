test_that("removing a transition twice preserves the other destinations", {
  rules <- createStateList(c("A", "B"))
  removed <- removeListVectorEl(rules, "A", "B")
  expect_identical(removed, list(A = "A", B = c("A", "B")))
  expect_identical(removeListVectorEl(removed, "A", "B"), removed)
  expect_identical(removeListVectorEl(removed, "Absent", "B"), removed)
  # An explicitly empty entry means no destinations, not an unrestricted list.
  expect_identical(removeListVectorEl(removed, "A", "A"),
                   list(A = character(0), B = c("A", "B")))
})

test_that("adding an existing transition does not duplicate it", {
  rules <- list(A = "A", B = c("A", "B"))
  added <- addListVectorEl(rules, "A", "B")
  expect_identical(added, createStateList(c("A", "B")))
  expect_identical(addListVectorEl(added, "A", "B"), added)
})
