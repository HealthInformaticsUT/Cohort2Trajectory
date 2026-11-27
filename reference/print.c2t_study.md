# Print a study or trajectory preview

Display a compact summary without changing study settings or trajectory
data. A study remains an environment, and trajectories remain a data
frame.

## Usage

``` r
# S3 method for class 'c2t_study'
print(x, ...)

# S3 method for class 'c2t_trajectories'
print(x, ..., n = 6L)
```

## Arguments

- x:

  A study returned by
  [`cohort2TrajectoryConfiguration()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md)
  or trajectories returned by
  [`createTrajectories()`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/createTrajectories.md).

- ...:

  Further arguments passed to the data-frame preview's print method.
  Unused when printing a study.

- n:

  Maximum number of trajectory rows to preview. A non-negative whole
  number; defaults to six.

## Value

The supplied object, invisibly.

## Details

The trajectory summary counts patients and distinct clinical labels
present in the returned rows. `START` and `EXIT` are boundary rows;
`OUT OF COHORT` rows are unresolved cycles. These labels are excluded
from the clinical-state count. Merged labels count as distinct states.
Episodic output also counts distinct patient-episode pairs and includes
`episode_id` in the preview.

The preview shows patient identifiers, state labels, start and end
dates, and time in cohort. All columns remain available through normal
data-frame operations. Use `as.data.frame(x)` for the ordinary full
data-frame display or `View(x)` to inspect the table. Column subsets
without the trajectory's patient, label or date columns use ordinary
data-frame printing.

Inspect a study's individual settings with `x$settingName`, or all
settings with `as.list(x, all.names = TRUE)`. Printing does not read
patient files or change the saved study.
