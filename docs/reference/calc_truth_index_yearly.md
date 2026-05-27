# Calculate "true" yearly index for a species

This function calculates the yearly index for a species by averaging
monthly index values.

## Usage

``` r
calc_truth_index_yearly(truth_index_monthly)
```

## Arguments

- truth_index_monthly:

  A tibble containing the monthly index data for a species. Returned
  from [`calc_truth_index_monthly()`](calc_truth_index_monthly.md).

## Value

A tibble containing the yearly index (e.g., biomass and catch) for the
specified species.

## Examples

``` r
data(ewe_ecosim_base_nwatlantic)
data <- ewe_ecosim_base_nwatlantic
truth_index_yearly <- ecosystemom:::calc_truth_monthly(
  data,
  species_name = "menhaden",
  truth_type = "biomass"
) |>
  ecosystemom:::calc_truth_index_monthly() |>
  ecosystemom:::calc_truth_index_yearly()
```
