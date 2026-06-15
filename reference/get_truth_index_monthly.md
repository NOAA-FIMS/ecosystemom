# Calculate "true" monthly index for a species

This function calculates the monthly index for a species by summing the
values across all ages by month.

## Usage

``` r
get_truth_index_monthly(truth_monthly)
```

## Arguments

- truth_monthly:

  A tibble containing the monthly "truth" data for a species. Returned
  from [`get_truth_monthly()`](get_truth_monthly.md).

## Value

A tibble containing the monthly index (e.g., biomass and catch) for the
specified species

## Examples

``` r
data(ewe_ecosim_base_nwatlantic)
data <- ewe_ecosim_base_nwatlantic
truth_index_monthly <- ecosystemom:::get_truth_monthly(
  data,
  species_name = "menhaden",
  truth_type = "biomass"
) |>
  ecosystemom:::get_truth_index_monthly()
```
