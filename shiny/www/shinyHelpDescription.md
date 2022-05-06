### Markov chains on OMOP data

**Development Status: Under Development**

**The results are not final**


### Information



This shiny application aims to help users in creating patient treatment trajectory datasets from cohorts generated from OMOP CDM data model.
This R package is still under development, bugs may occur.


### Abstract 

**Background:**  

The vast amount of electronically stored medical data offers a possibility to investigate patients' treatment trajectories. These trajectories give us a foundation to find out the best healthcare practices, evaluate the economics of treatment patterns and model the treatment paths.  This project implements Markov chains on such medical data. The aim of this work is to implement a technical solution for the aforementioned tasks. The gadget will be developed as an R package. The achieved tool is distributable to any database system operating on the open source OHDSI OMOP Common Data Model.

**Methods:** 

The R packages Cohort2Trajectory and TrajectoryMarkovAnalysis aim to describe patient treatment trajectories using OHDSI OMOP CDM integrated with discrete and continuous Markov chain models. Firstly, patient cohorts are imported. Secondly,  the imported cohort inclusion dataframe is altered and customised for supporting both types of Markov chain models. Thirdly, the Markov models are calculated. The models are used to assess the cost and survival rates of the treatment trajectories. The Markov models can also be used to generate patient trajectories. The models’ performance is also assessed with logrank tests. For all these phases a handy shiny GUI is provided, making the usage of the package easy.

**Findings:** 


**Interpretation:**



### Study Packages

- not published publicly at the moment


