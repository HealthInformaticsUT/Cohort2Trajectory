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
  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["SUBJECT_ID"];
  DateVector patientsStart = patientData["STATE_START_DATE"];
  DateVector patientsEnd = patientData["STATE_END_DATE"];
  std::vector<std::string> patientsStates = patientData["STATE"];
  std::vector<double> timeCohort = patientData["TIME_IN_COHORT"];
  
  for (int p=0; p < (int) patientIDs.size(); p++){
    
    // Getting patient id
    int patientID = patientIDs[p];
    int k = 0;
    // Let's get data of this person
    std::vector<int>::iterator iter = patientsIDs.begin();
    while ((iter = std::find(iter, patientsIDs.end(), patientID)) != patientsIDs.end())
    {
      int index = std::distance(patientsIDs.begin(), iter);
      std::string state = patientsStates[index];
      Date startDate = patientsStart[index];
      Date endDate = patientsEnd[index];
      
      if (k == 0){
      // Adding START state
      outpatientIDs.push_back(patientID);
      outStates.push_back("START");
      outStartDates.push_back(startDate);
      outEndDates.push_back(startDate);
      outtimeCohort.push_back(0);
      
      k ++;
      }
      
      // Add info to vectors
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(timeCohort[index]);
      
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
  }
  DataFrame outPatientData = DataFrame::create( Named("SUBJECT_ID") = outpatientIDs,
                                                _["STATE"] = outStates,
                                                _["STATE_START_DATE"] = outStartDates,
                                                _["STATE_END_DATE"] = outEndDates,
                                                _["TIME_IN_COHORT"] = outtimeCohort
                                                  );
  
  return outPatientData;
}
