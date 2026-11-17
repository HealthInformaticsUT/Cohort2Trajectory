# CRAN submission checks

This is a new submission.

## Completed checks

- Linux aarch64, Ubuntu 26.04.1 LTS, R 4.6.2 (2026-10-29): fresh source build and `R CMD check --as-cran --no-manual` using current CRAN dependencies.
- macOS Sequoia 15.8.1, aarch64, R 4.5.2: fresh source build and full `R CMD check --as-cran`, including the PDF manual.
- Examples, tests and vignettes passed on both platforms. No tests were skipped.
- No errors or warnings on either platform.

## Notes

- CRAN incoming feasibility reports `New submission`.
- The macOS HTML manual check reports that the system HTML Tidy is too old. Documentation generation and the PDF manual check passed. The Linux HTML manual check passed.
- The Linux compiler-flags note concerns `-Wdate-time`, `-Werror=format-security` and `-Wformat` supplied by the R image. The package has no Makevars and supplies no compiler flags. C++ compilation used the image's default g++ 15.2.0.

The package archive contains source code, documentation and synthetic fixtures. It contains no patient records, database binaries or credentials.

The repository also configures Windows and R-devel checks. Those checks have not yet run and are not claimed here as completed. The final source archives were built and checked on current R release.
