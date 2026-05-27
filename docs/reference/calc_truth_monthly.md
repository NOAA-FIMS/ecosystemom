# Calculate monthly "truth" for a species

This function calculates the monthly "truth" for a species by filtering
the data for the specified species and type

## Usage

``` r
calc_truth_monthly(data, species_name, truth_type)
```

## Arguments

- data:

  A tibble containing ecosystem model outputs from
  [`load_model()`](load_model.md).

- species_name:

  A string specifying the species name to subset.

- truth_type:

  A string specifying the type of "truth" to calculate.

## Value

A tibble containing the monthly "truth" data for the specified species.

## Examples

``` r
data(ewe_ecosim_base_nwatlantic)
data <- ewe_ecosim_base_nwatlantic
truth_monthly <- ecosystemom:::calc_truth_monthly(
  data,
  species_name = "menhaden",
  truth_type = "biomass"
)
```
