#include <Rcpp.h>
#include <map>
#include <set>
#include <cmath> // For time difference calculation

using namespace Rcpp;

// Function to calculate time in cohort (in years with 3 decimal precision)
double calculateTimeInCohort(Date start, Date end) {
  return std::round(((end - start + 1) / 365.25) * 1000) / 1000; // Convert to years, 3 decimal places
}

// [[Rcpp::export]]
DataFrame combineCohorts(DataFrame data, double threshold, std::vector<int> patientIDs) {
  
  std::vector<int> outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  std::vector<double> timeInCohort; // Store time spent in cohort
  
  std::vector<int> subjects = as<std::vector<int>>(data["subject_id"]);
  std::vector<std::string> ids = as<std::vector<std::string>>(data["cohort_definition_id"]);
  std::vector<Date> startDates = as<std::vector<Date>>(data["cohort_start_date"]);
  std::vector<Date> endDates = as<std::vector<Date>>(data["cohort_end_date"]);
  
  // Preserve original data
  std::vector<int> origPatientIDs = subjects;
  std::vector<std::string> origStates = ids;
  std::vector<Date> origStartDates = startDates;
  std::vector<Date> origEndDates = endDates;
  std::vector<double> origTimeInCohort;
  
  for (size_t i = 0; i < origStartDates.size(); i++) {
    origTimeInCohort.push_back(calculateTimeInCohort(origStartDates[i], origEndDates[i]));
  }
  
  for (int p = 0; p < (int)patientIDs.size(); p++) {
    int patientID = patientIDs[p];
    std::map<Date, std::set<std::string>> dateStates;
    
    // Step 1: Build the dateStates map
    for (size_t i = 0; i < subjects.size(); i++) {
      if (subjects[i] == patientID) {
        for (Date d = startDates[i]; d <= endDates[i]; d = d + 1) {
          dateStates[d].insert(ids[i]);
        }
      }
    }
    
    // Step 2: Iterate through the date states and merge appropriately
    Date currentStart = dateStates.begin()->first;
    std::set<std::string> currentState = dateStates.begin()->second;
    Date lastEnd = currentStart; // Track last known end date
    
    for (auto it = std::next(dateStates.begin()); it != dateStates.end(); ++it) {
      // If the state changes OR there is a **gap in days**, finalize the previous entry
      if (it->second != currentState || it->first > lastEnd + 1) {  
        // Convert current state set into a merged state string
        std::string combinedState = "";
        for (const auto& s : currentState) {
          if (!combinedState.empty()) combinedState += "+";
          combinedState += s;
        }
        
        // Save states **only** if they have been merged (contain "+")
        if (combinedState.find("+") != std::string::npos) {
          outpatientIDs.push_back(patientID);
          outStates.push_back(combinedState);
          outStartDates.push_back(currentStart);
          outEndDates.push_back(lastEnd);
          timeInCohort.push_back(calculateTimeInCohort(currentStart, lastEnd));
        }
        
        // Reset for the next period
        currentStart = it->first;
        currentState = it->second;
      }
      
      lastEnd = it->first; // Keep updating the last known end date
    }
    
    // Ensure the final detected state is stored correctly
    std::string finalState = "";
    for (const auto& s : currentState) {
      if (!finalState.empty()) finalState += "+";
      finalState += s;
    }
    
    if (finalState.find("+") != std::string::npos) {
      outpatientIDs.push_back(patientID);
      outStates.push_back(finalState);
      outStartDates.push_back(currentStart);
      outEndDates.push_back(lastEnd);
      timeInCohort.push_back(calculateTimeInCohort(currentStart, lastEnd));
    }
  }
  
  // Step 3: Append original (unmerged) cohort data back
  outpatientIDs.insert(outpatientIDs.begin(), origPatientIDs.begin(), origPatientIDs.end());
  outStates.insert(outStates.begin(), origStates.begin(), origStates.end());
  outStartDates.insert(outStartDates.begin(), origStartDates.begin(), origStartDates.end());
  outEndDates.insert(outEndDates.begin(), origEndDates.begin(), origEndDates.end());
  timeInCohort.insert(timeInCohort.begin(), origTimeInCohort.begin(), origTimeInCohort.end());
  
  return DataFrame::create(
    _["subject_id"] = wrap(outpatientIDs),
    _["cohort_definition_id"] = wrap(outStates),
    _["cohort_start_date"] = wrap(outStartDates),
    _["cohort_end_date"] = wrap(outEndDates),
    _["time_in_cohort"] = wrap(timeInCohort)
  );
}
