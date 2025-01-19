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
  NumericVector seqOrdinal;
  std::vector<double> outtimeCohort;
  
  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["subject_id"];
  DateVector patientsStart = patientData["state_start_date"];
  DateVector patientsEnd = patientData["state_end_date"];
  std::vector<std::string> patientsStates = patientData["state_label"];
  NumericVector stateIDs = patientData["state_id"];
  std::vector<double> timeCohort = patientData["time_in_cohort"];
  
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Getting patient id
    int patientID = patientIDs[p];
    
    std::string stateLast;
    int seqCounter = 1;
    
    // Let's get data of this person
    std::vector<int>::iterator iter = patientsIDs.begin();
    while ((iter = std::find(iter, patientsIDs.end(), patientID)) != patientsIDs.end())
    {
      int index = std::distance(patientsIDs.begin(), iter);
      std::string state = patientsStates[index];
      
      // Value for seqCounter;
      if (stateLast == state){
        seqCounter ++;
      }
      else {
        seqCounter = 1;
      }
      stateLast = state;
      
      // Add info to vectors
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(patientsStart[index]);
      outEndDates.push_back(patientsEnd[index]);
      outstateIDs.push_back(stateIDs[index]);
      outtimeCohort.push_back(timeCohort[index]);
      seqOrdinal.push_back(seqCounter);
      
      
      
      
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
        seqOrdinal.push_back(1);
        
        break;
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
                                                _["seq_ordinal"] = seqOrdinal);
  
  return outPatientData;
}
