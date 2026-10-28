test_that("saved definitions cannot escape the JSON folder", {
  root <- tempfile("c2t-definition-paths-")
  json <- file.path(root, "study", "JSON")
  dir.create(json, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  saved <- list(import = list(stateCohortLabels = "A"),
                definitions = list(target = "0", states = "A"),
                definitionContents = c("0" = "{}", A = "{}"))
  for (path in c("../outside", "states/../../outside", "/outside",
                 "C:/outside", "C:outside", "\\\\server\\share\\outside")) {
    saved$definitions$states <- path
    expect_error(.checkSavedDefinitions(saved, root, "study"), "relative paths.*without parent traversal")
  }
  saved$definitions$states <- "A"
  saved$definitionContents <- c("0" = "{}")
  expect_error(.stageSavedDefinitions(saved, json), "contents are incomplete")
  expect_length(list.files(json), 0)
})
