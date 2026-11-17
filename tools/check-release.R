# Run with Rscript --vanilla tools/check-release.R package-directory output-directory.
# This builds and checks locally. It never commits, uploads or submits anything.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Supply a package directory and a new persistent output directory.")
package_dir <- normalizePath(args[[1L]], mustWork = TRUE)
if (!file.exists(file.path(package_dir, "DESCRIPTION"))) stop("No DESCRIPTION in package directory.")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
  stop("Install the CRAN package rcmdcheck before running this script.")
}
output_dir <- args[[2L]]
if (file.exists(output_dir)) stop("Choose a new output directory, so earlier results are preserved.")
dir.create(output_dir, recursive = TRUE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session-info.txt"))
cat("Check output:", output_dir, "\n")
result <- rcmdcheck::rcmdcheck(path = package_dir, args = "--as-cran",
  build_args = character(), check_dir = output_dir, error_on = "never")
print(result)
# Notes need human review, including the normal first-submission note.
writeLines(c("ERRORS", result$errors, "WARNINGS", result$warnings, "NOTES", result$notes),
  file.path(output_dir, "check-summary.txt"))
if (!isTRUE(result$status == 0L) || isTRUE(result$timeout) ||
    length(result$errors) || length(result$warnings)) {
  stop("Package check did not finish successfully or reported errors/warnings. Review the logs.")
}
if (length(result$notes)) message("Review every NOTE before submission.")
archives <- list.files(output_dir, pattern = "[.]tar[.]gz$", full.names = TRUE, recursive = TRUE)
if (length(archives)) {
  print(tools::md5sum(archives))
  cat("Source archive:", archives, sep = "\n")
}
