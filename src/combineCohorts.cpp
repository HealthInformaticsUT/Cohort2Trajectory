#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calcOverlap(Date start1, Date end1, Date start2, Date end2) {
  Date start = std::max(start1, start2);
  Date end = std::min(end1, end2);
  double overlap = end - start;
  double interval1 = end1 - start1;
  if (overlap < 0) overlap = 0;
  return overlap / interval1;
}

// [[Rcpp::export]]
DataFrame mergeCohorts(DataFrame data, double threshold,  std::vector<int> patientIDs) {
  
  //Initiating dataframe output data
  NumericVector outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  
  std::vector<int> subjects = data["SUBJECT_ID"];
  std::vector<std::string> ids = data["COHORT_DEFINITION_ID"];
  DateVector startDates = data["COHORT_START_DATE"];
  DateVector endDates = data["COHORT_END_DATE"];
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Initiating patient input data, we'll get it from input vectors
    std::vector<Date> controlStart;
    std::vector<Date> controlEnd;
    std::vector<std::string> states;
    // Getting patient id
    int patientID = patientIDs[p];
    Rcout <<  patientID << "\n";
    // Let's get data of this person
    std::vector<int>::iterator iter = subjects.begin();
    while ((iter = std::find(iter, subjects.end(), patientID)) != subjects.end())
    {
      int index = std::distance(subjects.begin(), iter);
      states.push_back(ids[index]);
      controlStart.push_back(startDates[index]);
      controlEnd.push_back(endDates[index]);
      iter++;
    }
    
    int n = states.size();
    if (n < 2) {
      outpatientIDs.push_back(patientID);
      outStates.push_back(states[0]);
      outStartDates.push_back(controlStart[0]);
      outEndDates.push_back(controlEnd[0]);
      continue;
    }
    for (int i = 0; i < n - 1; ++i) {
      // We add every state, user can later choose the priority
      outpatientIDs.push_back(patientID);
      outStates.push_back(states[i]);
      outStartDates.push_back(controlStart[i]);
      outEndDates.push_back(controlEnd[i]);
      for (int j = i + 1; j < n; ++j) {
        double overlap = calcOverlap(controlStart[i], controlEnd[i], controlStart[j], controlEnd[j]);
        if (overlap > threshold) {
          std::string outId = states[i] + "+" + states[j];
          Date outStartDate = std::max(controlStart[i], controlStart[j]);
          Date outEndDate = std::min(controlEnd[i], controlEnd[j]);
          outpatientIDs.push_back(patientID);
          outStates.push_back(outId);
          outStartDates.push_back(outStartDate);
          outEndDates.push_back(outEndDate);
        }
      }
    }
  }
  return DataFrame::create(_["SUBJECT_ID"] = outpatientIDs, _["COHORT_DEFINITION_ID"] = outStates, _["COHORT_START_DATE"] = outStartDates, _["COHORT_END_DATE"] = outEndDates);
}

// [[Rcpp::export]]
DataFrame combineCohorts(DataFrame data, NumericVector threshold, std::vector<int> patientIDs) {
  int n = threshold.size();
  for (int i = 0; i < n; ++i) {
    Rcout <<  "THRESHOLD "<< threshold[i] << "\n";
    data = mergeCohorts(data, threshold[i], patientIDs);
  }
  return data;
}