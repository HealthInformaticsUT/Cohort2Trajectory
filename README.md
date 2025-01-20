
# Cohort2Trajectory

NOTE: After refactoring to CDMConnector the package execution code has significantly changed. You can find the old release v1.1.8 [here](https://github.com/HealthInformaticsUT/Cohort2Trajectory/releases/tag/v1.1.8). (January 2025)

# Introduction

Package Cohort2Trajectory creates patient orientated treatment trajectories from cohorts defined in OHDSI ATLAS. The package can be used with access to OMOP CDM database.
The package creates discrete and continuous time trajectories (and outputs them as .csv file) which describe patients' treatment through time.

For instructions and documentation please see the package's [wiki](https://healthinformaticsut.github.io/Cohort2Trajectory/) page.

# Features

**Sharing the study**: For sharing the trajectory generation parameters and cohorts just share the study folder and settings file with your counterpart.

# Technology

Cohort2Trajectory is an R package, with some functions implemented in C++.

For running the package with OMOP CDM, the user must have: 

1. Permissions to select on ohdsi cdm data schema.

2. Permissions to select on ohdsi cdm results schema.

3. Permissions to select, create and insert on ohdsi temp/scratch schema.

# License

Cohort2Trajectory is licensed under Apache License 2.0

# Development

Cohort2Trajectory is being developed in R Studio.

Feel free to contact the maintainer with questions: [markus.haug\@ut.ee](mailto:markus.haug@ut.ee){.email}

# See also 

[HealthInformaticsUT/CohortContrast](https://github.com/HealthInformaticsUT/CohortContrast)

[HealthInformaticsUT/TrajectoryMarkovAnalysis](https://github.com/HealthInformaticsUT/TrajectoryMarkovAnalysis)

[HealthInformaticsUT/TrajectoryViz](https://github.com/HealthInformaticsUT/TrajectoryViz)

# Publications

Markus Haug, Marek Oja, Maarja Pajusalu, Kerli Mooses, Sulev Reisberg, Jaak Vilo, Antonio Fernández Giménez, Thomas Falconer, Ana Danilović, Filip Maljkovic, Dalia Dawoud, Raivo Kolde, Markov modeling for cost-effectiveness using federated health data network, Journal of the American Medical Informatics Association, 2024;, ocae044, <https://doi.org/10.1093/jamia/ocae044>

# Citation

Haug, M.
(2023, May).
Cohort2Trajectory, v1.1.3.
GitHub.
<https://github.com/HealthInformaticsUT/Cohort2Trajectory/releases/tag/v1.1.8>

# Acknowledgements

Research group of Health-Informatics in University of Tartu <https://health-informatics.cs.ut.ee/>
