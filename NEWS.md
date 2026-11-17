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
  CRAN checks. The package remains licensed under Apache License 2.0.
