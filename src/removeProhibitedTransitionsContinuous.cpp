#include <Rcpp.h>
#include <string>
#include <cstring>
#include <algorithm>
#include "transitionMatching.h"
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
DataFrame removeProhibitedTransitionsContinuous(DataFrame patientData,
                                                std::vector<int> patientIDs,
                                                List allowedStatesList,
                                                Nullable<CharacterVector> absorbingStates = R_NilValue){
  
  
  //Initiating dataframe output data
  NumericVector outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  NumericVector outstateIDs;
  std::vector<double> outtimeCohort;
  NumericVector outSeqOrdinal;
  
  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["subject_id"];
  DateVector patientsStart = patientData["state_start_date"];
  DateVector patientsEnd = patientData["state_end_date"];
  std::vector<std::string> patientsStates = patientData["state_label"];
  NumericVector stateIDs = patientData["state_id"];
  std::vector<double> timeCohort = patientData["time_in_cohort"];
  // Match R's case conversion without changing labels or source-rule names.
  auto labelForms = c2t::transitionLabelForms(patientData["state_label"]);
  List foldedRules = c2t::foldTransitionRules(allowedStatesList);
  std::vector<std::string> absorbing;
  if (absorbingStates.isNotNull()) {
    absorbing = as<std::vector<std::string>>(absorbingStates.get());
  }
  
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Getting patient id
    int patientID = patientIDs[p];
    
    // Let's get data of this person
    std::vector<int>::iterator iter = patientsIDs.begin();
    std::string lastState;
    int seqCounter = 0;
    bool absorbed = false;
    
    while ((iter = std::find(iter, patientsIDs.end(), patientID)) != patientsIDs.end()) {
      int index = std::distance(patientsIDs.begin(), iter);
      std::string state = patientsStates[index];
      bool boundary = state == "START" || state == "EXIT";
      // Once an allowed absorbing state is retained, later clinical records
      // cannot affect the trajectory or require further transition rules.
      if (!boundary && absorbed) {
        iter++;
        continue;
      }
      bool permitted = true;
      // Boundary markers and the first clinical state need no source rule.
      // An empty list means unrestricted, as in discrete trajectories.
      if (!boundary && !lastState.empty() && allowedStatesList.size() > 0) {
        if (!allowedStatesList.containsElementNamed(lastState.c_str())) {
          stop("No allowed-transition entry for state '%s'.", lastState.c_str());
        }
        std::vector<std::string> destinations =
          as<std::vector<std::string>>(foldedRules[lastState]);
        permitted = std::find(destinations.begin(), destinations.end(), labelForms.at(state)) != destinations.end();
      }
      if (permitted) {
        seqCounter = !boundary && state == lastState ? seqCounter + 1 : 1;
        outpatientIDs.push_back(patientID);
        outStates.push_back(state);
        outStartDates.push_back(patientsStart[index]);
        outEndDates.push_back(patientsEnd[index]);
        outstateIDs.push_back(stateIDs[index]);
        outtimeCohort.push_back(timeCohort[index]);
        outSeqOrdinal.push_back(seqCounter);
        lastState = boundary ? "" : state;
        if (!boundary && std::find(absorbing.begin(), absorbing.end(), state) != absorbing.end()) {
          absorbed = true;
        }
      }
      // A rejected record does not change the last retained state.
      iter++;
    }
  }
  DataFrame outPatientData = DataFrame::create( Named("subject_id") = outpatientIDs,
                                                _["state_label"] = outStates,
                                                _["state_start_date"] = outStartDates,
                                                _["state_end_date"] = outEndDates,
                                                _["state_id"] = outstateIDs,
                                                _["time_in_cohort"] = outtimeCohort,
                                                _["seq_ordinal"] = outSeqOrdinal
  );
  
  return outPatientData;
}
