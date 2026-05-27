# Create Structural Equation Model (SEM) Equations and Time Series Data

This function prepares the inputs for a Dynamic Structural Equation
Model (DSEM). It generates the specific path equations (drivers to
targets) and compiles the corresponding time-series data based on
environmental links and diet composition thresholds.

## Usage

``` r
create_dsem_inputs(
  data,
  focal_functional_groups,
  diet_composition_threshold = 0.1
)
```

## Arguments

- data:

  A tibble/list containing `data_om`, `data_environment`, and
  `data_diet_composition`.

- focal_functional_groups:

  Character vector specifying one or more focal groups of interest
  (e.g., `c("menhaden 0", "meanhaden 1")`).

- diet_composition_threshold:

  Numeric. The minimum proportion of diet required to include a
  predator-prey link in the SEM. Default is `0.1`, which represent 10%
  of the diet.

## Value

A tibble with four columns:

- data_time_series_all:

  The complete matrix of available time series.

- data_time_series_sem:

  The subset of time series used specifically in the SEM.

- sem_tibble:

  A detailed tibble of drivers, targets, lags, and link types.

- sem_lines:

  A character string formatted for the `dsem` package.

## Details

The function constructs three types of linkages:

1.  **Environmental**: Links environmental indices to impacted
    functional groups.

2.  **Bottom-up**: Links prey biomass to the focal functional group.

3.  **Top-down**: Links predator biomass to the focal functional group.

## Examples

``` r
# Create mock data for demonstration
data_om <- tibble::tibble(
  year = rep(1984, 2),
  month = rep(1, 2),
  functional_group = c("Phytoplankton", "Menhaden (0yr)"),
  functional_group_snake_case = c("phytoplankton", "menhaden_0yr"),
  type = "biomass",
  value = c(100, 50)
)

data_environment <- tibble::tibble(
  index = "amo",
  year = 1984,
  month = 1,
  value = 0.679,
  unit = "NA",
  lag_months = 1,
  impacted_group = "Menhaden (0yr)",
  impacted_group_snake_case = "menhaden_0yr"
)

data_diet_composition <- tibble::tibble(
  prey = "Phytoplankton",
  predator = "Menhaden (0yr)",
  proportion = 0.5,
  prey_snake_case = "phytoplankton",
  predator_snake_case = "menhaden_0yr"
)

data <- list(data_om = list(data_om), data_environment = list(data_environment),
             data_diet_composition = list(data_diet_composition))

# Generate DSEM inputs
dsem_inputs <- create_dsem_inputs(data, "Menhaden (0yr)", 0.1)

# Extract the SEM equations for use with the dsem package
cat(dsem_inputs$sem_lines)
#> amo -> menhaden_0yr, 1, amo_menhaden_0yr
#> phytoplankton -> menhaden_0yr, 0, phytoplankton_menhaden_0yr

# Extract the time series data required for the SEM
print(dsem_inputs$data_time_series_sem[[1]])
#> Time Series:
#> Start = 1 
#> End = 1 
#> Frequency = 1 
#>   year month   amo phytoplankton menhaden_0yr
#> 1 1984     1 0.679           100           50
```
