# Tests for getCohorts() function located in R/importData.R

library(testthat)
#library(Eunomia)
library(Cohort2Trajectory)

# test_that("getCohorts return value and length", { 
#   
#   db <- DBI::dbConnect(duckdb::duckdb(),dbdir=CDMConnector::eunomia_dir("GiBleed"))
#   cdm <- CDMConnector::cdm_from_con(db, cdm_name="eunomia", cdm_schema = "main", write_schema="main")
#   
#   cohortSet <- CDMConnector::read_cohort_set(paste(getwd(), "/tests/testStudy/JSON/", sep = ""))
#   cdm <- CDMConnector::generateCohortSet(cdm, cohortSet, name = "cohort") 
#   cohorts <- getCohorts(cdm)
#   
#   expect_equal(cohorts,c(1,2,3))
#   expect_equal(length(cohorts),3)
# }) 
   