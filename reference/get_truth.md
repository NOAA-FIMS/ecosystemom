# Calculate "truth" for a species using ecosystem model outputs

The truth from an ecosystem model is often in different units than what
is needed to sample from. This function takes output data from
[`load_model()`](load_model.md) and calculates additional true
information such as converting monthly age-composition information into
annual data. No sampling is performed just mathematical operations.

## Usage

``` r
get_truth(data, species_name)
```

## Arguments

- data:

  A tibble containing ecosystem model (e.g., Ecopath with Ecosim)
  outputs from [`load_model()`](load_model.md).

- species_name:

  A string specifying the desired species you want to summarize
  information for. This string must match an entry in
  `data[["species"]]` and it currently cannot be a vector, just a single
  string. See the examples section for code on how to efficiently get
  output for multiple species.

## Value

A nested tibble where each row holds an output type (e.g., biomass,
catch, numbers, or weight) for a given time step category (e.g., monthly
or yearly). Thus, data within a row in returned output can be expanded
to reveal the full data, e.g., all months for an output type. The tibble
has the following columns:

- species_name:

  A character string with the name of the species.

- truth_label:

  A character string indicating the type of data, e.g., "biomass",
  "catch", "numbers", or "weight".

- truth_type:

  A character string specifying if the data is an index ("index") or
  age-structured ("agecomp").

- truth_time_step:

  A character string indicating the temporal resolution of the data,
  either "monthly" or "yearly".

- truth_om:

  A list-column containing the nested data frames with the actual
  "truth" values. Each data frame can be unnested and includes:

  year

  :   The year of the observation.

  month

  :   The month of the observation.

  unit

  :   The unit of the observation.

  value

  :   The numeric value of the observation.

  group

  :   The name of a group. The groups can contain age ranges, plus
      groups, or sub categories, e.g., juvenile. This is NA if
      truth_type is "index" instead of "agecomp".

## Details

The following values are calculated within this function:

- biomass

- catch

- numbers-at-age by dividing biomass-at-age by weight-at-age information

- weight

## Examples

``` r
data(ewe_ecosim_base_nwatlantic)
data <- ewe_ecosim_base_nwatlantic
truth <- get_truth(
  data,
  species_name = "menhaden"
)
#> ℹ The unit for numbers is currently set to "NA". Numbers need 
#> to be rescaled and filled in using `dplyr::mutate()` based on the underlying 
#> units of `biomass-at-age` and `weight-at-age`.

# To unnest tibble and view the data
truth_long <- tidyr::unnest(truth, cols = c(truth_om))

# To calculate truth for a vector of species using {purrr}
species_names <- c("menhaden", "striped bass", "bluefish")
truth <- purrr::map_dfr(
  species_names,
  function(x) get_truth(data, species_name = x)
)
#> ℹ The unit for numbers is currently set to "NA". Numbers need 
#> to be rescaled and filled in using `dplyr::mutate()` based on the underlying 
#> units of `biomass-at-age` and `weight-at-age`.
#> ℹ The unit for numbers is currently set to "NA". Numbers need 
#> to be rescaled and filled in using `dplyr::mutate()` based on the underlying 
#> units of `biomass-at-age` and `weight-at-age`.
#> ℹ The unit for numbers is currently set to "NA". Numbers need 
#> to be rescaled and filled in using `dplyr::mutate()` based on the underlying 
#> units of `biomass-at-age` and `weight-at-age`.
```
