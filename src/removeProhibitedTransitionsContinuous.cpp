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
  std::vector<int> patientsIDs = patientData["SUBJECT_ID"];
  DateVector patientsStart = patientData["STATE_START_DATE"];
  DateVector patientsEnd = patientData["STATE_END_DATE"];
  std::vector<std::string> patientsStates = patientData["STATE"];
  NumericVector stateIDs = patientData["STATE_ID"];
  std::vector<double> timeCohort = patientData["TIME_IN_COHORT"];
  NumericVector seqOrdinal = patientData["SEQ_ORDINAL"];
  
  
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
        // Rcpp::Rcout << allowedStates << '\n';
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
  DataFrame outPatientData = DataFrame::create( Named("SUBJECT_ID") = outpatientIDs,
                                                _["STATE"] = outStates,
                                                _["STATE_START_DATE"] = outStartDates,
                                                _["STATE_END_DATE"] = outEndDates,
                                                _["STATE_ID"] = outstateIDs,
                                                _["TIME_IN_COHORT"] = outtimeCohort,
                                                _["SEQ_ORDINAL"] = outSeqOrdinal
  );
  
  return outPatientData;
}