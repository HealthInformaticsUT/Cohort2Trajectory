test_that("saved definitions cannot escape the JSON folder", {
  root <- tempfile("c2t-definition-paths-")
  json <- file.path(root, "study", "JSON")
  dir.create(json, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  writeLines("{}", file.path(json, "0.json"))
  writeLines("{}", file.path(root, "outside.json"))
  saved <- list(import = list(stateCohortLabels = "A"),
                definitions = list(target = "0", states = "A"))
  for (path in c("../outside", "states/../../outside", "/outside",
                 "C:/outside", "C:outside", "\\\\server\\share\\outside")) {
    saved$definitions$states <- path
    expect_error(.checkSavedDefinitions(saved, root, "study"), "relative paths.*without parent traversal")
  }
  # Symbolic-link creation on Windows may require privileges not held by CI.
  if (.Platform$OS.type != "windows") {
    expect_true(file.symlink(file.path(root, "outside.json"), file.path(json, "A.json")))
    saved$definitions$states <- "A"
    expect_error(.checkSavedDefinitions(saved, root, "study"), "must resolve inside the JSON folder")
  }
})
