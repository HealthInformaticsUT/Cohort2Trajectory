#include <Rcpp.h>
#include <string>
#include <cstring>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
DataFrame removeAfterAbsorbingStatesContinuous(DataFrame patientData,
                                std::vector<int> patientIDs,
                                std::vector<std::string> absorbingStates){
  
  
  //Initiating dataframe output data
  NumericVector outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  NumericVector outstateIDs;
  std::vector<double> outtimeCohort;
  
  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["SUBJECT_ID"];
  DateVector patientsStart = patientData["STATE_START_DATE"];
  DateVector patientsEnd = patientData["STATE_END_DATE"];
  std::vector<std::string> patientsStates = patientData["STATE"];
  NumericVector stateIDs = patientData["STATE_ID"];
  std::vector<double> timeCohort = patientData["TIME_IN_COHORT"];
  
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Getting patient id
    int patientID = patientIDs[p];
    
    // Let's get data of this person
    std::vector<int>::iterator iter = patientsIDs.begin();
    while ((iter = std::find(iter, patientsIDs.end(), patientID)) != patientsIDs.end())
    {
      int index = std::distance(patientsIDs.begin(), iter);
      std::string state = patientsStates[index];

      
      // Add info to vectors
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(patientsStart[index]);
      outEndDates.push_back(patientsEnd[index]);
      outstateIDs.push_back(stateIDs[index]);
      outtimeCohort.push_back(timeCohort[index]);
      
      
      
      
      // If it is an absorbing state break loop
      
      // We have to explicitly add 'EXIT' state as it would be otherwise cut off
      if (std::find(absorbingStates.begin(), absorbingStates.end(), state) != absorbingStates.end())
      {
        while (patientsStates[index] != "EXIT"){
          index ++;
        }
        // Adding EXIT state
        outpatientIDs.push_back(patientID);
        outStates.push_back(patientsStates[index]);
        outStartDates.push_back(patientsStart[index]);
        outEndDates.push_back(patientsEnd[index]);
        outstateIDs.push_back(stateIDs[index]);
        outtimeCohort.push_back(timeCohort[index]);
        
        break;
      }
      
      iter++;
    }
  }
  DataFrame outPatientData = DataFrame::create( Named("SUBJECT_ID") = outpatientIDs,
                                                _["STATE"] = outStates,
                                                _["STATE_START_DATE"] = outStartDates,
                                                _["STATE_END_DATE"] = outEndDates,
                                                _["STATE_ID"] = outstateIDs,
                                                _["TIME_IN_COHORT"] = outtimeCohort);
  
  return outPatientData;
}
