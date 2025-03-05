#include <Rcpp.h>
#include <limits>
#include <string>
#include <cstring>
#include <algorithm>
#include <cmath>
#include <iostream>
using namespace Rcpp;
using namespace std;


int daysOverlap(Date dateStart,
                Date dateEnd,
                Date controlStart,
                Date controlEnd) {
  int ans;
  if (dateStart > controlStart &&
      dateEnd > controlEnd) {
    ans = controlEnd - dateStart;
  }
  else if (dateStart <= controlStart &&
           dateEnd > controlEnd) {
    ans = controlEnd - controlStart + 1;
  }
  else if (dateStart <= controlStart &&
           dateEnd <= controlEnd) {
    ans = dateEnd - controlStart + 1;
  }
  else {
    ans = dateEnd - dateStart;
  }
  if (ans > 0){return ans;}
  return 0;
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
                  Rcpp::List allowedStatesList){

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
  
  for (int p=0; p < (int) patientIDs.size(); p++){

  // Initiating patient input data, we'll get it from input vectors
  std::vector<Date> controlStart;
  std::vector<Date> controlEnd;
  std::vector<std::string> states;
  std::vector<double> times;


  // Getting patient id
  int patientID = patientIDs[p];
  // Let's get data of this person
  std::string lastLegalState = "$$not_intialized_yet$$";

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
  // Get earliest date, the controlStart values are ordered
  Date cohortstartDate = controlStart[0];
  Date cohortendDate = *std::max_element(controlEnd.begin(), controlEnd.end()) + 1;
  // # Let's get the number of possible state durations in the timespan of endDate - startDate
  // # This is needed for mapping possible states to time intervals
  int totalStateDurations =  (int) ceil((cohortendDate - cohortstartDate)/stateDuration);
  if (1 > totalStateDurations){
    totalStateDurations = 1;
  }
  int vectorLength = (int) controlStart.size();
  if(stateSelection == 1){
    for (int iteration=1; iteration <= totalStateDurations; iteration++) {

      NumericVector daysOverlapVector;
      std::string state;
      std::string lastState = "$$not_intialized_yet$$";
      double time = (iteration - 1)*stateDuration/365.25;
      Date startDate = cohortstartDate + (iteration - 1)*stateDuration;
      Date endDate = startDate + stateDuration;

      for (int i=0; i < vectorLength; i++){
        daysOverlapVector.push_back(daysOverlap(startDate,
                                                endDate,
                                                controlStart[i],
                                                            controlEnd[i]));
      }
      //Rcpp::Rcout << as<Rcpp::StringVector>(daysOverlapVector) << '\n';
      int sumV = sum(daysOverlapVector);
      if(sumV == 0) {
        if (oocFix == "None"){
        state = "OUT OF COHORT";
        }
        else if (oocFix == "Last present state"){
          if (lastLegalState == "$$not_intialized_yet$$") {
            lastLegalState =  "OUT OF COHORT";
          }
          state = lastLegalState;
        }
        else{
          state = oocFix;
        }
        }
      else {
        // # Let's calculate the shift in days, all shifts in the past according to startingDate will be given a value of -Inf
        NumericVector daysShift = dateDiff(startDate, controlStart);
        // as.numeric(as.Date(startDate) -as.Date(personData$cohort_start_date))
        daysShift = replaceOnTreshold(daysShift,0,std::numeric_limits<int>::min());
        NumericVector daysShift_copy = daysShift;
 
        //std::sort(daysShift_copy.begin(), daysShift_copy.end(), greater<int>()); // Sort an array of in greatest-first order.
        
        
        Rcpp::StringVector allowedStates;
        if(lastState == "$$not_intialized_yet$$"){
          allowedStates = statePriorityVector;
        }
        else{
          Rcpp::StringVector allowedStates = as<Rcpp::StringVector>(allowedStatesList[lastState]);
        }
        
        for (int value=0; value < vectorLength; value++){
        
        auto it =  std::find(daysShift.begin(), daysShift.end(), daysShift[value]);
        
        int index = it - daysShift.begin();
        // Rcpp::Rcout << as<Rcpp::StringVector>(daysShift) << '\n';
         int maxElementIndex = std::max_element(daysShift.begin(),daysShift.end()) - daysShift.begin();
         // Rcpp::Rcout << maxElementIndex << '\n';
        // indexMax = which.max(daysShift)
         state = states[maxElementIndex];
        //state = states[index];
        
        // auto isPresent = std::find(allowedStates.begin(), allowedStates.end(), state);
        std::string lowercaseState = state;
        std::transform(lowercaseState.begin(), lowercaseState.end(), lowercaseState.begin(), ::tolower);
        
        bool isPresent = false;
        int numAllowedStates = allowedStates.size();
        
        for (int i = 0; i < numAllowedStates; i++) {
          std::string allowedState = Rcpp::as<std::string>(allowedStates[i]);
          std::string lowercaseAllowedState = allowedState;
          std::transform(lowercaseAllowedState.begin(), lowercaseAllowedState.end(), lowercaseAllowedState.begin(), ::tolower);
          
          if (lowercaseAllowedState == lowercaseState) {
            isPresent = true;
            break;
          }
        }
        
        if (isPresent){
          lastState = state;
          break;
        }
        else if (it == daysShift.end()){
          
          if (oocFix == "None"){
            state = "OUT OF COHORT";
          }
          else if (oocFix == "Last present state"){
            if (lastLegalState == "$$not_intialized_yet$$") {
              lastLegalState =  "OUT OF COHORT";
            }
            state = lastLegalState;
          }
          else{
            state = oocFix;
          }          break;
        }
        
        // state = personData$cohort_definition_id[indexMax]
        }
      }
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(time);
      lastLegalState = state;
    }

  }
  else if(stateSelection == 2){
    for (int iteration=1; iteration <= totalStateDurations; iteration++) {

      NumericVector daysOverlapVector;
      std::string state;
      std::string lastState = "$$not_intialized_yet$$";
      double time = (iteration - 1)*stateDuration/365.25;
      Date startDate = cohortstartDate + (iteration - 1)*stateDuration;
      Date endDate = startDate + stateDuration;



      // Let's choose the state that overlaps the most with the present date interval
      // The function daysOverlap is defined in Helpers.R
      for (int i=0; i < vectorLength; i++){
        daysOverlapVector.push_back(daysOverlap(startDate,
                                                endDate,
                                                controlStart[i],
                                                            controlEnd[i]));
      }
      
      int sumV = sum(daysOverlapVector);
      if(sumV == 0) {
        if (oocFix == "None"){
          state = "OUT OF COHORT";
        }
        else if (oocFix == "Last present state"){
          if (lastLegalState == "$$not_intialized_yet$$") {
            lastLegalState =  "OUT OF COHORT";
          }
          state = lastLegalState;
        }
        else{
          state = oocFix;
        }
        }
      else {
        
        Rcpp::StringVector allowedStates;
        if(lastState == "$$not_intialized_yet$$"){
          allowedStates = statePriorityVector;
        }
        else{
          Rcpp::StringVector allowedStates = as<Rcpp::StringVector>(allowedStatesList[lastState]);
        }

        for (int value=0; value < vectorLength; value++){
          
          int maxElementIndex = std::max_element(daysOverlapVector.begin(),daysOverlapVector.end()) - daysOverlapVector.begin();
          state = states[maxElementIndex];
          
          std::string lowercaseState = state;
          std::transform(lowercaseState.begin(), lowercaseState.end(), lowercaseState.begin(), ::tolower);
          
          bool isPresent = false;
          int numAllowedStates = allowedStates.size();
          
          for (int i = 0; i < numAllowedStates; i++) {
            std::string allowedState = Rcpp::as<std::string>(allowedStates[i]);
            std::string lowercaseAllowedState = allowedState;
            std::transform(lowercaseAllowedState.begin(), lowercaseAllowedState.end(), lowercaseAllowedState.begin(), ::tolower);
            
            if (lowercaseAllowedState == lowercaseState) {
              isPresent = true;
              break;
            }
          }
          // auto isPresent = std::find(allowedStates.begin(), allowedStates.end(), state);
          // Rcpp::Rcout << (isPresent != allowedStates.end()) << '\n';
          //if (isPresent != allowedStates.end()){
          if (isPresent){
            lastState = state;
            break;
          }
          else {
            daysOverlapVector[maxElementIndex] = 0;
            if (sum(daysOverlapVector) == 0){
              if (oocFix == "None"){
                state = "OUT OF COHORT";
              }
              else if (oocFix == "Last present state"){
                if (lastLegalState == "$$not_intialized_yet$$") {
                  lastLegalState =  "OUT OF COHORT";
                }
                state = lastLegalState;
              }
              else{
                state = oocFix;
              }
            break;
            }
          }
          
        }
      }
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(time);
      lastLegalState = state;
    }

  }
  else if (stateSelection == 3) {
    for (int iteration=1; iteration <= totalStateDurations; iteration++) {

      NumericVector daysOverlapVector;
      std::string state;
      std::string lastState = "$$not_intialized_yet$$";
      double time = (iteration - 1)*stateDuration/365.25;
      Date startDate = cohortstartDate + (iteration - 1)*stateDuration;
      Date endDate = startDate + stateDuration;


      // Let's implement the prioritization of state cohorts.
      // This means that we will select the state which is present in the given timespan and has the highest priority
      for (int i=0; i < vectorLength; i++){
        daysOverlapVector.push_back(daysOverlap(startDate,
                                                endDate + (-1),
                                                controlStart[i],
                                                            controlEnd[i]));
      }
      
      int sumV = sum(daysOverlapVector);
      //Rcpp::Rcout << daysOverlapVector << '\n';
      if(sumV == 0) {
        if (oocFix == "None"){
          state = "OUT OF COHORT";
        }
        else if (oocFix == "Last present state"){
          if (lastLegalState == "$$not_intialized_yet$$") {
            lastLegalState =  "OUT OF COHORT";
          }
          state = lastLegalState;
        }
        else{
          state = oocFix;
        }
      }
      else {
        Rcpp::StringVector allowedStates;
        if(lastState == "$$not_intialized_yet$$"){
          allowedStates = statePriorityVector;
        }
        else{
          Rcpp::StringVector allowedStates = as<Rcpp::StringVector>(allowedStatesList[lastState]);
        }
        // Rcpp::Rcout << allowedStates;
        int maxPriorityIndex = std::numeric_limits<int>::max();
        for (int j=0; j < vectorLength; j++){
          if (daysOverlapVector[j] > 0){
            std::vector<std::string>::iterator it = std::find(statePriorityVector.begin(), statePriorityVector.end(), states[j]);
            int index_result = std::distance(statePriorityVector.begin(), it);
            // int index_result = index_result - statePriorityVector.end();
            std::string candidateState;
            candidateState = statePriorityVector[index_result];
            
            //auto isPresent = std::find(allowedStates.begin(), allowedStates.end(), candidateState);
            std::string lowercaseState = candidateState;
            std::transform(lowercaseState.begin(), lowercaseState.end(), lowercaseState.begin(), ::tolower);
            
            bool isPresent = false;
            int numAllowedStates = allowedStates.size();
            
            for (int i = 0; i < numAllowedStates; i++) {
              std::string allowedState = Rcpp::as<std::string>(allowedStates[i]);
              std::string lowercaseAllowedState = allowedState;
              std::transform(lowercaseAllowedState.begin(), lowercaseAllowedState.end(), lowercaseAllowedState.begin(), ::tolower);
              
              if (lowercaseAllowedState == lowercaseState) {
                isPresent = true;
                break;
              }
            }
            
            if(index_result < maxPriorityIndex && isPresent){
              maxPriorityIndex = index_result;
              state = candidateState;
            }
            else {
            daysOverlapVector[j] = 0;
            if (sum(daysOverlapVector) == 0){
              if (oocFix == "None"){
                state = "OUT OF COHORT";
              }
              else if (oocFix == "Last present state"){
                if (lastLegalState == "$$not_intialized_yet$$") {
                  lastLegalState =  "OUT OF COHORT";
                  }
                state = lastLegalState;
              }
              else{
                state = oocFix;
              }
              break;
            }
            }
          }
          if (j == vectorLength) {
            lastState = state;
            break;
          }
        }
      }
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(time);
      lastLegalState = state;
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
