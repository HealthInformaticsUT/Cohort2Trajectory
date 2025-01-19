#include <Rcpp.h>
#include <string>
#include <cstring>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
DataFrame removeAfterAbsorbingStatesDiscrete(DataFrame patientData,
                                std::vector<int> patientIDs,
                                std::vector<std::string> absorbingStates){
  
  //Initiating dataframe output data
  NumericVector outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  std::vector<double> outtimeCohort;
  NumericVector seqOrdinal;
  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["subject_id"];
  DateVector patientsStart = patientData["state_start_date"];
  DateVector patientsEnd = patientData["state_end_date"];
  std::vector<std::string> patientsStates = patientData["state_label"];
  std::vector<double> timeCohort = patientData["time_in_cohort"];
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Getting patient id
    int patientID = patientIDs[p];
    
    std::string stateLast;
    int seqCounter = 1;
    int k = 0;
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
      Date startDate = patientsStart[index];
      Date endDate = patientsEnd[index];
      
      if (k == 0){
      // Adding START state
      outpatientIDs.push_back(patientID);
      outStates.push_back("START");
      outStartDates.push_back(startDate);
      outEndDates.push_back(startDate);
      outtimeCohort.push_back(0);
      seqOrdinal.push_back(1);
      
      k ++;
      }
      
      // Add info to vectors
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(timeCohort[index]);
      seqOrdinal.push_back(seqCounter);
      
      // If it is an absorbing state break loop
      if (std::find(absorbingStates.begin(), absorbingStates.end(), state) != absorbingStates.end())
      {
        break;
      }
      
      iter++;
    }
    // Adding EXIT state
    outpatientIDs.push_back(patientID);
    outStates.push_back("EXIT");
    outStartDates.push_back(outEndDates.back() + 1);
    outEndDates.push_back(outEndDates.back() + 1);
    outtimeCohort.push_back(outtimeCohort.back());
    seqOrdinal.push_back(1);
  }
  DataFrame outPatientData = DataFrame::create( Named("subject_id") = outpatientIDs,
                                                _["state_label"] = outStates,
                                                _["state_start_date"] = outStartDates,
                                                _["state_end_date"] = outEndDates,
                                                _["time_in_cohort"] = outtimeCohort,
                                                _["seq_ordinal"] = seqOrdinal
                                                  );
  
  return outPatientData;
}
