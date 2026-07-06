# ecosystemom

Ecosystem operating model. Generates data sets from ecosystem model
scenarios for stock assessment models.

## Installing ecosystemom

Install the most recent version of FIMSdiags using the following code:

``` r

install.packages("remotes")
remotes::install_github("NOAA-FIMS/ecosystemom")
```

or using {pak}

``` r

pak::pak("NOAA-FIMS/ecosystemom")
```

## Why are we building {ecosystemom}?

- Connect ecosystem model outputs to stock assessment simulation testing
  through a flexible operating model framework.
- Support evaluation and comparison of FIMS and other assessment methods
  against ecosystem model outputs under realistic ecosystem dynamics.
- Enable research on ecosystem-informed stock assessment approaches.
- Foster collaboration between ecosystem modelers and stock assessment
  scientists.

🤝 **Contributions and feedback are welcome!** If you’re interested in
this project or its applications, please interact with this repository
or contact Bai Li at <bai.li@noaa.gov>

## Functions

The package includes functions below:

- [`load_model()`](reference/load_model.md) — import and standardize
  output from ecosystem operating models (OM). For example, this
  function can be used to read and standardize Ecopath with Ecosim (EwE)
  Ecosim outputs for downstream analyses within {ecosystemom}.
- [`get_truth()`](reference/get_truth.md) — extract “true” population
  quantities from the OM, including annual or monthly biomass
  trajectories and biomass-at-age.
- `sample_*()` — generate sampled observations from OM outputs that
  incorporate sampling and observation error for use in estimation
  models such as the Fisheries Integrated Modeling System (FIMS).
- [`create_dsem_inputs()`](reference/create_dsem_inputs.md) — prepare
  environmental covariates or diet composition data from the OM to
  support candidate model specifications for dynamic structural equation
  models (DSEMs) and related ecosystem-informed analyses.

## Pkgdown site

The pkgdown site is temporarily being built from the `add-a-demo` branch
and is currently under active development.
