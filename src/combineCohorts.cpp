#include <Rcpp.h>
#include <map>
#include <set>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame combineCohorts(DataFrame data, double threshold, std::vector<int> patientIDs) {
  
  std::vector<int> outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  
  std::vector<int> subjects = as<std::vector<int>>(data["subject_id"]);
  std::vector<std::string> ids = as<std::vector<std::string>>(data["cohort_definition_id"]);
  std::vector<Date> startDates = as<std::vector<Date>>(data["cohort_start_date"]);
  std::vector<Date> endDates = as<std::vector<Date>>(data["cohort_end_date"]);
  
  // Preserve original data
  std::vector<int> origPatientIDs = subjects;
  std::vector<std::string> origStates = ids;
  std::vector<Date> origStartDates = startDates;
  std::vector<Date> origEndDates = endDates;
  
  for (int p = 0; p < (int)patientIDs.size(); p++) {
    int patientID = patientIDs[p];
    std::map<Date, std::set<std::string>> dateStates;
    
    for (size_t i = 0; i < subjects.size(); i++) {
      if (subjects[i] == patientID) {
        for (Date d = startDates[i]; d <= endDates[i]; d = d + 1) {
          dateStates[d].insert(ids[i]);
        }
      }
    }
    
    Date currentStart = dateStates.begin()->first;
    std::set<std::string> currentState = dateStates.begin()->second;
    
    for (auto it = std::next(dateStates.begin()); it != dateStates.end(); ++it) {
      if (it->second != currentState) {
        std::string combinedState = "";
        for (const auto& s : currentState) {
          if (!combinedState.empty()) combinedState += "+";
          combinedState += s;
        }
        
        if (combinedState.find("+") != std::string::npos) {
          outpatientIDs.push_back(patientID);
          outStates.push_back(combinedState);
          outStartDates.push_back(currentStart);
          outEndDates.push_back(std::prev(it)->first);
        }
        
        currentStart = it->first;
        currentState = it->second;
      }
    }
    
    std::string finalState = "";
    for (const auto& s : currentState) {
      if (!finalState.empty()) finalState += "+";
      finalState += s;
    }
    
    if (finalState.find("+") != std::string::npos) {
      outpatientIDs.push_back(patientID);
      outStates.push_back(finalState);
      outStartDates.push_back(currentStart);
      outEndDates.push_back(dateStates.rbegin()->first);
    }
  }
  
  // Append original data back
  outpatientIDs.insert(outpatientIDs.begin(), origPatientIDs.begin(), origPatientIDs.end());
  outStates.insert(outStates.begin(), origStates.begin(), origStates.end());
  outStartDates.insert(outStartDates.begin(), origStartDates.begin(), origStartDates.end());
  outEndDates.insert(outEndDates.begin(), origEndDates.begin(), origEndDates.end());
  
  return DataFrame::create(
    _["subject_id"] = wrap(outpatientIDs),
    _["cohort_definition_id"] = wrap(outStates),
    _["cohort_start_date"] = wrap(outStartDates),
    _["cohort_end_date"] = wrap(outEndDates)
  );
}
