Cohort2Trajectory
======================

Introduction
============

Package Cohort2Trajectory creates patient orientated treatment trajectories from cohorts defined in OHDSI ATLAS.
The package can be used with access to OMOP CDM database.
The package creates discrete and continuous time trajectories (and outputs them as .csv file) which describe patients' treatment through time.
Package can be run with GUI or CLI.

Features
========
**Sharing the study**: For sharing the trajectory generation parameters and cohorts just share the "inst" folder with your counterpart. When placing the "inst" directory to the pathToResults folder the settings are shared (given that the same studyName argument is used). When using GUI one should use the "Import via JSONs" tab for loading saved cohorts.

Screenshots
===========

Technology
==========
Cohort2Trajectory is an R package, with some functions implemented in C++.

For running the package with OMOP CDM, the user must have:
1. Permissions to select on ohdsi cdm data schema.
2. Permissions to select on ohdsi cdm results schema.
3. Permissions to select, create and insert on ohdsi temp/scratch schema.

System Requirements
===================

Getting Started
===============

Package manual: [Cohort2Trajectory.pdf](https://github.com/HealthInformaticsUT/Cohort2Trajectory/blob/main/Cohort2Trajectory_1.0.pdf)

Package guide: [Cohort2Trajectory_guide.pdf](https://github.com/HealthInformaticsUT/Cohort2Trajectory/blob/main/Cohort2Trajectory_guide_1.0.pdf)
 
License
=======
Cohort2Trajectory is licensed under Apache License 2.0

Development
===========
Cohort2Trajectory is being developed in R Studio. Feel free to contact the maintainer with questions: markus.haug@ut.ee

Citation
===========
Haug, M. (2023, May). Cohort2Trajectory, v1.1.3. GitHub. https://github.com/HealthInformaticsUT/Cohort2Trajectory/releases/tag/v1.1.3


# Acknowledgements

Research group of Health-Informatics in University of Tartu https://health-informatics.cs.ut.ee/

