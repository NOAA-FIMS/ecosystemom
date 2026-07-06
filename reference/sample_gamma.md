# Sample using a gamma distribution

Sample from a gamma distribution using the expected value in `x` and the
standard deviation in `sd`. The gamma shape and scale parameters are
derived from the input mean and standard deviation.

## Usage

``` r
sample_gamma(x, sd = 1)
```

## Arguments

- x:

  A numeric vector of observations to sample from. If a single value is
  provided, it will be recycled to the length of `sd`. Otherwise, it
  must be the same length as `sd`.

- sd:

  A numeric vector of standard deviations to use in
  [`rgamma()`](https://rdrr.io/r/stats/GammaDist.html). If a single
  value is provided, it will be recycled to the length of `x`.
  Otherwise, it must be the same length as `x`.

## Value

A numeric vector of the same length as `x` is returned.

## Details

The gamma distribution is parameterized by shape and scale. Given an
expected value `x` and a standard deviation `sd`, the shape parameter is
calculated as `x^2 / sd^2` and the scale parameter as `sd^2 / x`.

## Examples

``` r
set.seed(123)
sample_gamma(1, sd = 1)
#> [1] 0.1822171
set.seed(123)
sample_gamma(1, sd = c(0, 1))
#> [1] 1.0000000 0.1822171
sample_gamma(1:5, sd = 1)
#> [1] 1.6957511 0.5263902 2.9603714 5.7468949 5.3669060
```
