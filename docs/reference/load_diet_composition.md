# Load diet composition data

This function loads diet composition data from a CSV file and
standardizes it into a tidy format. It automatically detects the format
of the input file based on its column names and processes it
accordingly.

## Usage

``` r
load_diet_composition(file_path, verbose = TRUE)
```

## Arguments

- file_path:

  The path to the CSV file containing the diet composition data.

- verbose:

  Logical. If `TRUE` (default), prints informative messages.

## Value

A tibble in a long format with the following columns:

- `year`: The year of the observation (NA for static Ecopath data).

- `prey`: The name of the prey functional group.

- `predator`: The name of the predator functional group.

- `proportion`: The proportion of the prey in the predator's diet.

- `prey_snake_case`: The prey name converted to snake_case.

- `predator_snake_case`: The predator name converted to snake_case.

## Details

The function currently supports three formats from Ecopath with Ecosim
(EwE):

- **Ecospace Yearly**: Identified by a "Year" column. Data represents
  biomass consumed, which is converted to proportions.

- **Ecospace Monthly**: Identified by a "TimeStep" column. This format
  is recognized but not yet supported.

- **Ecopath**: Identified by a "Prey \\ predator" column. This is a
  static diet matrix. The function also handles empty column names that
  can sometimes appear in EwE outputs by ignoring them.
