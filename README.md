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

System Requirements
===================

Getting Started
===============

Package manual: [Cohort2Trajectory.pdf](https://github.com/HealthInformaticsUT/Cohort2Trajectory/blob/main/Cohort2Trajectory_1.0.pdf)
 
License
=======
Cohort2Trajectory is licensed under Apache License 2.0

Development
===========
Cohort2Trajectory is being developed in R Studio.

# Acknowledgements


