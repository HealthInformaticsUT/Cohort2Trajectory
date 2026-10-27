#include <Rcpp.h>
#include <limits>
#include <string>
#include <cstring>
#include <algorithm>
#include <cmath>
#include <iostream>
#include <cctype>
#include <map>
using namespace Rcpp;
using namespace std;


int daysOverlap(Date dateStart,
                Date dateEnd,
                Date controlStart,
                Date controlEnd) {
  // Cohort intervals include both their start and end dates.
  Date overlapStart = std::max(dateStart, controlStart);
  Date overlapEnd = std::min(dateEnd, controlEnd);
  return std::max(0, static_cast<int>(overlapEnd - overlapStart) + 1);
}

NumericVector dateDiff(Date referenceDate, std::vector<Date> dateVector){
  NumericVector daysDiffVector;
  for (int i=0; i < (int) dateVector.size(); i++) {
    Date dateFromVector = dateVector[i];
    daysDiffVector.push_back(int (referenceDate - dateFromVector));
  }
  return daysDiffVector;
}

NumericVector replaceOnTreshold(NumericVector referenceVector, int treshold, int replaceInt){
  for (int i=0; i < (int) referenceVector.size(); i++) {
    int checkedValue = referenceVector[i];
    if (checkedValue > treshold){
      referenceVector[i] = replaceInt;
    }
      }
  return referenceVector;
}

// [[Rcpp::export]]
DataFrame getDiscreteStates(int stateSelection,
                  std::string oocFix, 
                  int stateDuration,
                  std::vector<int> patientIDs,
                  DataFrame patientData,
                  std::vector<std::string> statePriorityVector,
                  Rcpp::List allowedStatesList,
                  Nullable<CharacterVector> absorbingStates = R_NilValue){

  if (stateDuration < 1) {
    stop("stateDuration must be a positive number of days.");
  }
  if (stateSelection < 1 || stateSelection > 3) {
    stop("stateSelection must be 1 (First), 2 (Overlap), or 3 (Priority).");
  }
  std::vector<std::string> absorbing;
  if (absorbingStates.isNotNull()) {
    absorbing = as<std::vector<std::string>>(absorbingStates.get());
  }

  //Initiating dataframe output data
  NumericVector outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  std::vector<double> outtimeCohort;
  
  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["subject_id"];
  
  DateVector patientsStart = patientData["cohort_start_date"];
  
  DateVector patientsEnd = patientData["cohort_end_date"];
  
  std::vector<std::string> patientsStates = patientData["cohort_definition_id"];
  std::vector<double> timeCohort = patientData["time_in_cohort"];

  // Priority is a ranking (also used for First/Overlap ties), not a filter.
  // Validate before allowed-destination filtering can hide unlisted records.
  // The R caller has already bounded these records to the target intervals.
  for (const auto& state : patientsStates) {
    if (state != "0" &&
        std::find(statePriorityVector.begin(), statePriorityVector.end(), state) == statePriorityVector.end()) {
      stop("State '%s' is missing from statePriorityVector (stateCohortPriorityOrder in the study configuration).",
           state.c_str());
    }
  }
  
  for (int p=0; p < (int) patientIDs.size(); p++){

  // Initiating patient input data, we'll get it from input vectors
  std::vector<Date> controlStart;
  std::vector<Date> controlEnd;
  std::vector<std::string> states;
  std::vector<double> times;


  // Getting patient id
  int patientID = patientIDs[p];
  // Let's get data of this person

  std::vector<int>::iterator iter = patientsIDs.begin();
  while ((iter = std::find(iter, patientsIDs.end(), patientID)) != patientsIDs.end())
  {
    int index = std::distance(patientsIDs.begin(), iter);
    states.push_back(patientsStates[index]);
    controlStart.push_back(patientsStart[index]);
    controlEnd.push_back(patientsEnd[index]);
    times.push_back(timeCohort[index]);
    iter++;
  }
  if (controlStart.empty()) {
    stop("No input rows for requested internal patient ID %d. Derive patientIDs from the supplied data.", patientID);
  }
  // Target rows preserve the full span even when clinical states are sparse.
  Date cohortstartDate = *std::min_element(controlStart.begin(), controlStart.end());
  Date cohortendDate = *std::max_element(controlEnd.begin(), controlEnd.end());
  // # Let's get the number of possible state durations in the timespan of endDate - startDate
  // # This is needed for mapping possible states to time intervals
  int totalStateDurations =  (int) ceil((cohortendDate - cohortstartDate + 1)/stateDuration);
  if (1 > totalStateDurations){
    totalStateDurations = 1;
  }
  // Remember the last selected clinical state across cycles. An unresolved
  // OUT OF COHORT cycle does not erase the transition constraints.
  std::string lastState;
  for (int iteration = 1; iteration <= totalStateDurations; iteration++) {
    double time = (iteration - 1) * stateDuration / 365.25;
    Date startDate = cohortstartDate + (iteration - 1) * stateDuration;
    Date endDate = std::min(startDate + (stateDuration - 1), cohortendDate);

    std::vector<std::string> allowedStates = statePriorityVector;
    if (!lastState.empty() && allowedStatesList.size() > 0) {
      if (!allowedStatesList.containsElementNamed(lastState.c_str())) {
        stop("No allowed-transition entry for state '%s'.", lastState.c_str());
      }
      allowedStates = as<std::vector<std::string>>(allowedStatesList[lastState]);
    }
    auto isAllowed = [&](const std::string& candidate) {
      std::string lowerCandidate = candidate;
      std::transform(lowerCandidate.begin(), lowerCandidate.end(),
                     lowerCandidate.begin(), [](unsigned char c) { return std::tolower(c); });
      for (std::string allowed : allowedStates) {
        std::transform(allowed.begin(), allowed.end(), allowed.begin(),
                       [](unsigned char c) { return std::tolower(c); });
        if (allowed == lowerCandidate) return true;
      }
      return false;
    };

    int best = -1;
    int bestPriority = std::numeric_limits<int>::max();
    // Group by priority index so equal state coverage retains priority order.
    std::map<int, std::vector<std::pair<Date, Date>>> overlapIntervals;
    for (size_t i = 0; i < states.size(); i++) {
      // Target rows define follow-up, but are not clinical state candidates.
      if (states[i] == "0") continue;
      int overlap = daysOverlap(startDate, endDate, controlStart[i], controlEnd[i]);
      if (overlap <= 0 || !isAllowed(states[i])) continue;

      auto priorityIt = std::find(statePriorityVector.begin(), statePriorityVector.end(), states[i]);
      int priority = std::distance(statePriorityVector.begin(), priorityIt);
      if (stateSelection == 2) {
        overlapIntervals[priority].push_back(std::make_pair(
          std::max(startDate, controlStart[i]), std::min(endDate, controlEnd[i])));
        continue;
      }
      bool better = best < 0;
      if (best >= 0) {
        if (stateSelection == 1) {
          better = controlStart[i] < controlStart[best] ||
            (controlStart[i] == controlStart[best] && priority < bestPriority);
        } else {
          better = priority < bestPriority;
        }
      }
      if (better) {
        best = i;
        bestPriority = priority;
      }
    }

    std::string state = "OUT OF COHORT";
    if (best >= 0) {
      state = states[best];
    }
    int bestOverlap = -1;
    for (auto& entry : overlapIntervals) {
      auto& intervals = entry.second;
      std::sort(intervals.begin(), intervals.end());
      Date coveredStart = intervals.front().first;
      Date coveredEnd = intervals.front().second;
      int coveredDays = 0;
      // Count the union, not the sum, so repeated records cannot add days.
      for (size_t i = 1; i < intervals.size(); i++) {
        if (intervals[i].first <= coveredEnd) {
          coveredEnd = std::max(coveredEnd, intervals[i].second);
        } else {
          coveredDays += static_cast<int>(coveredEnd - coveredStart) + 1;
          coveredStart = intervals[i].first;
          coveredEnd = intervals[i].second;
        }
      }
      coveredDays += static_cast<int>(coveredEnd - coveredStart) + 1;
      if (coveredDays > bestOverlap) {
        bestOverlap = coveredDays;
        state = statePriorityVector[entry.first];
      }
    }
    if (state == "OUT OF COHORT") {
      if (oocFix == "Last present state") {
        // Carry-forward must not introduce a prohibited self-transition.
        if (!lastState.empty() && isAllowed(lastState)) state = lastState;
      } else if (oocFix != "None") {
        // A named fallback is a state too, and must respect the same rules.
        if (lastState.empty() || allowedStatesList.size() == 0 || isAllowed(oocFix)) {
          state = oocFix;
        }
      }
    }

    if (state != "OUT OF COHORT") lastState = state;
    outpatientIDs.push_back(patientID);
    outStates.push_back(state);
    outStartDates.push_back(startDate);
    outEndDates.push_back(endDate);
    outtimeCohort.push_back(time);
    // Stop at the selected absorbing cycle, before later cycles can demand
    // outgoing rules. Raw but prohibited/unselected absorbing records do not
    // end follow-up. The outer loop must still process the next patient.
    if (std::find(absorbing.begin(), absorbing.end(), state) != absorbing.end()) {
      break;
    }
  }
  }
  DataFrame outPatientData = DataFrame::create( Named("subject_id") = outpatientIDs,
                                                _["state_label"] = outStates,
                                                _["state_start_date"] = outStartDates,
                                                _["state_end_date"] = outEndDates,
                                                _["time_in_cohort"] = outtimeCohort);

  return outPatientData;
}
