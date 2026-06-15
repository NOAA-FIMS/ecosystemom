# Load environmental data from a CSV file

Load environmental data from a CSV file

## Usage

``` r
load_csv_environmental_data(file_path, lag_months, impacted_group)
```

## Arguments

- file_path:

  A string. Path to the CSV file containing the environmental data. The
  CSV file must contain the following columns: - `index`: Name of the
  environmental index (character). - `year`: Year corresponding to each
  value (integer). - `month`: Month corresponding to each value
  (integer). - `value`: Value of the index (numeric). - `unit`: Unit of
  the index (character).

- lag_months:

  An integer. The lag between environmental index and functional group,
  in months. For example, if lag_months = 12, the environmental effect
  is assumed to influence the functional group with a 12-month delay.

- impacted_group:

  A string indicating the functional group impacted by the environmental
  index.

## Value

A tibble containing the environmental data with lag and functional group
information.

## Examples

``` r
data <- load_csv_environmental_data(
  file_path = file.path(
    system.file("extdata", package = "ecosystemom"),
    "ewe_ecosim_with_environmental_data_nwatlantic", "amo_lag1.csv"
  ),
  lag_months = 12,
  impacted_group = "menhaden 0"
)
```
