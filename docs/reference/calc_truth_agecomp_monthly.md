# Calculate "true" monthly age composition for a species

This function calculates the monthly age composition for a species by
selecting the relevant columns and filtering the data for the specified
species.

## Usage

``` r
calc_truth_agecomp_monthly(truth_monthly)
```

## Arguments

- truth_monthly:

  A tibble containing the monthly "truth" data for a species. Returned
  from [`calc_truth_monthly()`](calc_truth_monthly.md).

## Value

A tibble containing the monthly age composition (e.g., biomass, catch,
and weight) for the specified species.

## Examples

``` r
data(ewe_ecosim_base_nwatlantic)
data <- ewe_ecosim_base_nwatlantic
truth_agecomp_monthly <- ecosystemom:::calc_truth_monthly(
  data,
  species_name = "menhaden",
  truth_type = "biomass"
) |>
  ecosystemom:::calc_truth_agecomp_monthly()
```
