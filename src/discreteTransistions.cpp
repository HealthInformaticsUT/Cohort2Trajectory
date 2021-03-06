#include <Rcpp.h>
#include <limits>
#include <string>
#include <cstring>
#include <algorithm>
#include <cmath>
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
                  int stateDuration,
                  std::vector<int> patientIDs,
                  DataFrame patientData,
                  std::vector<std::string> statePriorityVector){

  //Initiating dataframe output data
  NumericVector outpatientIDs;
  std::vector<std::string> outStates;
  std::vector<Date> outStartDates;
  std::vector<Date> outEndDates;
  std::vector<double> outtimeCohort;

  // Getting data from patienData DataFrame
  std::vector<int> patientsIDs = patientData["SUBJECT_ID"];
  DateVector patientsStart = patientData["COHORT_START_DATE"];
  DateVector patientsEnd = patientData["COHORT_END_DATE"];
  std::vector<std::string> patientsStates = patientData["COHORT_DEFINITION_ID"];
  std::vector<double> timeCohort = patientData["TIME_IN_COHORT"];

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
  // Get earliest date, the controlStart values are ordered
  Date cohortstartDate = controlStart[0];
  Date cohortendDate = *std::max_element(controlEnd.begin(), controlEnd.end());
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
      double time = (iteration - 1)*stateDuration/365.25;
      Date startDate = cohortstartDate + (iteration - 1)*stateDuration;
      Date endDate = startDate + stateDuration;

      for (int i=0; i < vectorLength; i++){
        daysOverlapVector.push_back(daysOverlap(startDate,
                                                endDate,
                                                controlStart[i],
                                                            controlEnd[i]));
      }
      int sumV = sum(daysOverlapVector);
      if(sumV == 0) {
        state = "OUT OF COHORT";
        }
      else {
        // # Let's calculate the shift in days, all shifts in the past according to startingDate will be given a value of -Inf
        NumericVector daysShift = dateDiff(startDate, controlStart);
        // as.numeric(as.Date(startDate) -as.Date(personData$COHORT_START_DATE))
        daysShift = replaceOnTreshold(daysShift,0,std::numeric_limits<int>::min());
        int maxElementIndex = std::max_element(daysShift.begin(),daysShift.end()) - daysShift.begin();
        // indexMax = which.max(daysShift)
        state = states[maxElementIndex];;
        // state = personData$COHORT_DEFINITION_ID[indexMax]
      }
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(time);
    }

  }
  else if(stateSelection == 2){
    for (int iteration=1; iteration <= totalStateDurations; iteration++) {

      NumericVector daysOverlapVector;
      std::string state;
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
        state = "OUT OF COHORT";
        }
      else {
        int maxElementIndex = std::max_element(daysOverlapVector.begin(),daysOverlapVector.end()) - daysOverlapVector.begin();
        state = states[maxElementIndex];
      }
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(time);
    }

  }
  else if (stateSelection == 3) {
    for (int iteration=1; iteration <= totalStateDurations; iteration++) {

      NumericVector daysOverlapVector;
      std::string state;
      double time = (iteration - 1)*stateDuration/365.25;
      Date startDate = cohortstartDate + (iteration - 1)*stateDuration;
      Date endDate = startDate + stateDuration;


      // Let's implement the prioritization of state cohorts.
      // This means that we will select the state which is present in the given timespan and has the highest priority
      for (int i=0; i < vectorLength; i++){
        daysOverlapVector.push_back(daysOverlap(startDate,
                                                endDate,
                                                controlStart[i],
                                                            controlEnd[i]));
      }
      int sumV = sum(daysOverlapVector);
      if(sumV == 0) {state = "OUT OF COHORT";}
      else {
        // # Let's calculate the shift in days, all shifts in the past according to startingDate will be given a value of -Inf
        int maxPriorityIndex = std::numeric_limits<int>::max();
        for (int j=0; j < vectorLength; j++){
          if (daysOverlapVector[j] > 0){
            std::vector<std::string>::iterator it = std::find(statePriorityVector.begin(), statePriorityVector.end(), states[j]);
            int index_result = std::distance(statePriorityVector.begin(), it);
            // int index_result = index_result - statePriorityVector.end();
            if(index_result < maxPriorityIndex){
              maxPriorityIndex = index_result;
              state = statePriorityVector[index_result];
            }
          }
        }
      }
      outpatientIDs.push_back(patientID);
      outStates.push_back(state);
      outStartDates.push_back(startDate);
      outEndDates.push_back(endDate);
      outtimeCohort.push_back(time);
    }
    }
  }
  DataFrame outPatientData = DataFrame::create( Named("SUBJECT_ID") = outpatientIDs,
                                                _["STATE"] = outStates,
                                                _["STATE_START_DATE"] = outStartDates,
                                                _["STATE_END_DATE"] = outEndDates,
                                                _["TIME_IN_COHORT"] = outtimeCohort);

  return outPatientData;
}
