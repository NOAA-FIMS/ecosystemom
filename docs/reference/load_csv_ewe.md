# Load in EwE monthly output data

This function loads and standardizes output data from Ecopath with
Ecosim (EwE) models.

## Usage

``` r
load_csv_ewe(file_path, model_years, functional_groups, verbose = TRUE)
```

## Arguments

- file_path:

  The path to the CSV file containing the EwE output data.

- model_years:

  A numeric vector of years corresponding to the model run. TODO: This
  could be derived automatically from an annual data file in the future.

- functional_groups:

  A vector of names of the functional groups in the model.

- verbose:

  Logical. If `TRUE` (default), prints informative messages.

## Examples

``` r
if (FALSE) { # \dontrun{
# The following example is not run by default because these files are only
# included in the GitHub clone of the repository and not in the package data
load_csv_ewe(
  file_path = fs::path(
    "data-raw", "ewe_nwatlantic", "base_run", "biomass_monthly.csv"
  ),
  model_years = 1985:2017,
  functional_groups = get_functional_groups(
    file_path = fs::path(
      base_run_dir, "basic_estimates.csv"
    )
  )
)
} # }
```
