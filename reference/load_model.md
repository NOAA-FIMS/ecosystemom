# Load an ecosystem model

Load the necessary files from an ecosystem model and return a single,
long data frame of information.

## Usage

``` r
load_model(
  ...,
  type = c("ewe_ecosim", "ewe_ecospace", "atlantis"),
  verbose = TRUE
)
```

## Arguments

- ...:

  Arguments that are passed onto lower level `load_model_()*` functions.
  Such as those needed for `load_model_ewe_ecosim()`, which are
  `directory` and `functional_groups`.

- type:

  A string indicating which type of model data you want to load. The
  default is ewe_ecosim. Strings should be all lower case text.

- verbose:

  Logical. If `TRUE` (default), prints informative messages.

## Value

A tibble is returned that matches the structure of
[ewe_ecosim_base_nwatlantic](ewe_ecosim_base_nwatlantic.md).
