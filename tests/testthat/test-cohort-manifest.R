test_that("manifest updates never remove a neighbouring study's file", {
  root <- tempfile("c2t-manifest-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "studies")
  name <- "stroke"
  intended <- file.path(path, name, "JSON", "CohortsToCreate.csv")
  neighbour <- file.path(paste0(path, name), "JSON", "CohortsToCreate.csv")
  dir.create(dirname(intended), recursive = TRUE)
  dir.create(dirname(neighbour), recursive = TRUE)
  for (parent in c(path, paste0(path, "/"))) {
    writeLines("Unrelated study manifest", neighbour)
    before <- tools::md5sum(neighbour)
    writeLines("Previous manifest to replace", intended)
    manifest <- generateCohortSetCSV(c("0", "1"), "A", parent, name)
    expect_identical(tools::md5sum(neighbour), before)
    expect_equal(read.csv(intended), manifest)
  }
})
