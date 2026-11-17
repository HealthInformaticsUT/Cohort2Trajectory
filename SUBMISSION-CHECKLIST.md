# Submission preparation

Run `Rscript --vanilla tools/check-release.R . /path/to/new-check-folder` from the repository. The folder must be new. The script builds the source archive, runs `R CMD check --as-cran` with the installed R version, and keeps its logs. It does not commit or submit anything. Install `rcmdcheck` first if needed.

Before submission:

- Complete the final study-server test with the exact source archives prepared for release.
- Confirm the package version, author credits, maintainer address and licence.
- Review the check results on current R release and R-devel, including Windows. The repository workflow checks these after a push. A workflow file alone is not evidence that the checks passed.
- Review all NOTES. First-submission and network-availability notes must be explained accurately in `cran-comments.md`. Do not describe tests that were not run.
- Check that the archive contains source code, documentation and synthetic fixtures only, with no credentials, patient records, binary libraries or local outputs.
- Check any reverse dependencies if this is an update to an existing CRAN package.
- Submit the tarball created by `R CMD build` through the CRAN form, then confirm the maintainer email. Keep the tested tarball and its checksum.

The server analysis validates the study integration. It does not replace package checks on supported R platforms. CRAN acceptance remains a review decision.

References: [CRAN policy](https://cran.r-project.org/web/packages/policies.html), [submission checklist](https://cran.r-project.org/web/packages/submission_checklist.html).
