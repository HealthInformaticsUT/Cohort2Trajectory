#ifndef COHORT2TRAJECTORY_TRANSITION_MATCHING_H
#define COHORT2TRAJECTORY_TRANSITION_MATCHING_H

#include <Rcpp.h>
#include <map>
#include <string>
#include <vector>

namespace c2t {

// Use the same locale-aware conversion as R validation and state selection.
// UTF-8 comparison forms are separate from original labels and rule keys.
inline Rcpp::CharacterVector foldTransitionLabels(SEXP labels) {
  Rcpp::Function lower = Rcpp::Environment::base_env()["tolower"];
  Rcpp::Function utf8 = Rcpp::Environment::base_env()["enc2utf8"];
  return utf8(lower(labels));
}

inline Rcpp::List foldTransitionRules(const Rcpp::List& rules) {
  Rcpp::List folded = Rcpp::clone(rules);
  for (R_xlen_t i = 0; i < rules.size(); ++i) {
    folded[i] = foldTransitionLabels(rules[i]);
  }
  return folded;
}

inline std::map<std::string, std::string> transitionLabelForms(Rcpp::CharacterVector labels) {
  Rcpp::CharacterVector distinct = Rcpp::unique(labels);
  std::vector<std::string> originals = Rcpp::as<std::vector<std::string>>(distinct);
  std::vector<std::string> folded = Rcpp::as<std::vector<std::string>>(foldTransitionLabels(distinct));
  std::map<std::string, std::string> forms;
  for (size_t i = 0; i < originals.size(); ++i) forms[originals[i]] = folded[i];
  return forms;
}

} // namespace c2t

#endif
