# Cohort2Trajectory 1.3.0.9000

* Nonmerged generation now rejects unknown transition destinations before
  writing outputs. Configured but unobserved states, observed local states
  and named discrete gap fallbacks remain valid, with case-insensitive matching.

* Named `atlasStateCohorts` supplies state labels alongside ATLAS IDs or local
  JSON filenames, without a separate `stateCohortLabels` vector.

* Use `cycleLength` instead of `lengthOfStay` in configuration and generation.
  New settings snapshots use version 5. Version 4 snapshots still load without
  rewriting the saved file.
* Returned trajectories carry their cycle length, state labels and absorbing
  states for direct use in TMA. The definition includes unobserved configured
  states and resolved merged labels. RDS preserves it; CSV contains rows only.
* Study and trajectory objects print compact summaries while retaining all
  settings and data.

# Cohort2Trajectory 1.3.0

* The public workflow uses readable option names, structured merged-state
  membership and a portable RDS settings file containing the cohort definitions.
  Settings CSV files, numeric selection codes and merge thresholds are no longer
  supported. Reconfigure older studies before running this version.
* Patient identifiers, observation boundaries, partial cycles and clinical
  labels are retained consistently across batches and saved-study reruns.
* ATLAS definitions can be downloaded with optional runtime HTTP headers or
  read from local JSON. Downloads no longer need ROhdsiWebApi or browser login.
* CDM imports use unique generated tables and remove them after collection.
  Caller-owned cohort tables and database connections are left in place.
  Trajectory generation no longer uploads an unused table named `test`.
* Cleaned input is saved as `Data/importedDataCleaned.csv`, so nested JSON paths
  cannot change the output path. Invalid study paths fail before writing data.
* Native patient loops can be interrupted and do not change R's random-number
  state. Tests and examples use synthetic data and temporary folders.
* Updated package metadata, offline articles and source-package contents for
  package checks. The package remains licensed under Apache License 2.0.
