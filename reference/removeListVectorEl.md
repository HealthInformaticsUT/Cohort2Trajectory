# Remove an allowed transition

Remove an allowed transition

## Usage

``` r
removeListVectorEl(stateList, transitionHead, transitionTail)
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

Removing an absent transition leaves the list unchanged. Removing the
last destination preserves an empty source entry, meaning no allowed
destinations. Destination matching is case-insensitive using R's
locale-aware case conversion, as in trajectory generation. All matching
entries are removed. Source names still require exact spelling. The
input list is not modified in place.
