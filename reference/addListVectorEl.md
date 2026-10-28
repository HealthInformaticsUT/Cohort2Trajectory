# Add an allowed transition

Add an allowed transition

## Usage

``` r
addListVectorEl(stateList, transitionHead, transitionTail)
```

## Arguments

- stateList:

  List of all the states storing the possible destination states

- transitionHead:

  State label which is the state where the transition is coming from

- transitionTail:

  State label which is the state where the transition is going to

## Value

The updated transition list.

## Details

Destinations are deduplicated case-insensitively using R's locale-aware
case conversion, as in trajectory generation. The first spelling and
order are preserved and new destinations are appended. Source names
still require exact spelling. The input list is not modified in place.
