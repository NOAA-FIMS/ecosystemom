# Calculate "true" yearly age composition for a species

This function calculates the yearly age composition for a species by
averaging the monthly age composition values.

## Usage

``` r
get_truth_agecomp_yearly(truth_agecomp_monthly)
```

## Arguments

- truth_agecomp_monthly:

  A tibble containing the monthly age composition data for a species.
  Returned from
  [`get_truth_agecomp_monthly()`](get_truth_agecomp_monthly.md).

## Value

A tibble containing the yearly age composition (e.g., biomass, catch,
and weight) for the specified species.

## Examples

``` r
data(ewe_ecosim_base_nwatlantic)
data <- ewe_ecosim_base_nwatlantic
truth_agecomp_yearly <- ecosystemom:::get_truth_monthly(
  data,
  species_name = "menhaden",
  truth_type = "biomass"
) |>
  ecosystemom:::get_truth_agecomp_monthly() |>
  ecosystemom:::get_truth_agecomp_yearly()
```
