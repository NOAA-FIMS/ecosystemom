# Estimate true maturity parameters from ecosystem model input

Estimate true maturity parameters from ecosystem model input

## Usage

``` r
estimate_true_maturity(ages, spawning_proportion, functional_form = "logistic")
```

## Arguments

- ages:

  A vector of ages.

- spawning_proportion:

  A numeric vector of spawning proportions (0 to 1) for each functional
  group within a species. The values can be obtained from Ecopath -\>
  Input -\> Basic input -\> Edit Multi_Stanza Groups.

- functional_form:

  A character string specifying the curve type. Currently supports
  "logistic".

## Value

A named list containing the estimated 'inflection_point' and 'slope'.
