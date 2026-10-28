# Override the behaviour of one merged state

Define an exception to the rules inherited by a combination of states.
Add the result to `mergedStateOverrides` in a study configured with
`mergeStates = TRUE`. This definition does not create patient intervals.

## Usage

``` r
mergedState(members, allowedTransitions, absorbing, label = NULL)
```

## Arguments

- members:

  Character vector containing at least two distinct original state
  labels. Use exact labels, not merged display labels or state keys.
  Order does not matter. Labels must not be blank or missing.

- allowedTransitions:

  Optional replacement destinations. A character vector names individual
  states. A list specifies sets of original members, so
  `list("C", c("C", "D"))` permits both C alone and the combination of C
  and D. `character(0)` or [`list()`](https://rdrr.io/r/base/list.html)
  permits no destinations. Omit the argument to inherit transitions.
  `NULL` is not accepted.

- absorbing:

  Optional `TRUE` or `FALSE` replacing inherited absorbing status. Omit
  it to inherit. `FALSE` lets the trajectory continue even when an
  original member is absorbing.

- label:

  Optional display label. Supply one nonempty, non-missing string. It
  must not conflict with an original or another explicit display label,
  even when ignoring case. `"0"`, `"START"`, `"EXIT"` and
  `"OUT OF COHORT"` are reserved, including case variants.

## Value

A definition of class `c2t_merged_state` for the study's
`mergedStateOverrides` list.

## Membership and labels

The analyst's original state definitions determine membership. Each
source or destination member must refer to an original state. Supply at
most one override for each membership set. By default, display labels
join the member labels with `" + "`. Generated label collisions are
disambiguated. Labels are never parsed to recover membership, so an
original label containing a plus sign remains a single original state.

## Inherited rules

Without an explicit destination list, allowed destinations come from the
union of the original source members' permissions. Every member newly
added at a destination must be permitted by at least one source member.
For a self-transition or a transition that only removes members,
permission is required for every retained destination member. An empty
original `allowedStatesList` permits all original members.

A combination is absorbing if any member is absorbing, unless
`absorbing` overrides that rule. Absorbing status ends follow-up even
when outgoing destinations are listed. An explicit destination list
replaces the inherited list without changing absorbing status.

The highest-priority member determines a combination's priority. Larger
combinations come first when that priority is equal, followed by state
key order. This helper does not override priority.

The resolved membership, label and rules are available in
`studyEnv$stateDictionary` and `Data/stateDictionary.rds` after
generation. Completed study settings retain the original rules and these
overrides for a fresh saved-study import.

## See also

[`cohort2TrajectoryConfiguration`](https://healthinformaticsut.github.io/Cohort2Trajectory/reference/cohort2TrajectoryConfiguration.md)

## Examples

``` r
mergedState(c("A", "B"), allowedTransitions = "C", absorbing = FALSE)
mergedState(c("A", "B"), absorbing = FALSE)
mergedState(c("A", "B"), allowedTransitions = list("C", c("C", "D")))
```
