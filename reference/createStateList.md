# Allow every state to transition to every state

Allow every state to transition to every state

## Usage

``` r
createStateList(stateVector)
```

## Arguments

- stateVector:

  Character vector of state labels.

## Value

A named list allowing every state to transition to every state.

## Examples

``` r
createStateList(c("Stable", "Complication", "Death"))
#> $Stable
#> [1] "Stable"       "Complication" "Death"       
#> 
#> $Complication
#> [1] "Stable"       "Complication" "Death"       
#> 
#> $Death
#> [1] "Stable"       "Complication" "Death"       
#> 
```
