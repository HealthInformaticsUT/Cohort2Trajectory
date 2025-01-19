#include <Rcpp.h>
#include <string>
#include <cstring>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
DataFrame removeProhibitedTransitionsContinuous(DataFrame patientData,
                                                std::vector<int> patientIDs,
                                                List allowedStatesList){
  
  
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
  NumericVector seqOrdinal = patientData["seq_ordinal"];
  
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Getting patient id
    int patientID = patientIDs[p];
    
    // Let's get data of this person
    std::vector<int>::iterator iter = patientsIDs.begin();
    std::string lastState; 
    
    while ((iter = std::find(iter, patientsIDs.end(), patientID)) != patientsIDs.end()) {
      int index = std::distance(patientsIDs.begin(), iter);
      std::string state = patientsStates[index];
      
      
      // If state is "START" or "EXIT" then move on to the next state
      
      if (state == "START" || state == "EXIT") {
        lastState = "$$not_intialized_yet$$";
        iter++;
        // Add info to vectors
        outpatientIDs.push_back(patientID);
        outStates.push_back(state);
        outStartDates.push_back(patientsStart[index]);
        outEndDates.push_back(patientsEnd[index]);
        outstateIDs.push_back(stateIDs[index]);
        outtimeCohort.push_back(timeCohort[index]);
        outSeqOrdinal.push_back(seqOrdinal[index]);
        continue;
      }
      else if (lastState == "$$not_intialized_yet$$") {
        lastState = state;
        // Add info to vectors
        outpatientIDs.push_back(patientID);
        outStates.push_back(state);
        outStartDates.push_back(patientsStart[index]);
        outEndDates.push_back(patientsEnd[index]);
        outstateIDs.push_back(stateIDs[index]);
        outtimeCohort.push_back(timeCohort[index]);
        outSeqOrdinal.push_back(seqOrdinal[index]);
      }
      // If there is a forbidden transition we do not add the targeted state
      else {
        // siin miski broken
        Rcpp::StringVector allowedStates = as<Rcpp::StringVector>(allowedStatesList[lastState]);
        //std::string nextState = patientsStates[index+1];
        // Control: is the next state allowed
        auto isPresent = std::find(allowedStates.begin(), allowedStates.end(), state);
        if(isPresent != allowedStates.end()){
          // Add info to vectors
          outpatientIDs.push_back(patientID);
          outStates.push_back(state);
          outStartDates.push_back(patientsStart[index]);
          outEndDates.push_back(patientsEnd[index]);
          outstateIDs.push_back(stateIDs[index]);
          outtimeCohort.push_back(timeCohort[index]);
          outSeqOrdinal.push_back(seqOrdinal[index]);
          
          lastState = state;
        }
      }
      
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