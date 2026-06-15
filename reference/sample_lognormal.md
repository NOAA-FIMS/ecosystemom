# Sample using a lognormal distribution

Sample from a standard normal in log space and apply this exponentiated
observation error to the observations, i.e, `x`, after the simulated
error values are bias corrected.

## Usage

``` r
sample_lognormal(x, sd = 1)
```

## Arguments

- x:

  A numeric vector of observations to sample from. These observations
  should be on the normal scale. If a single value is provided, it will
  be recycled to the length of `sd`. Otherwise, it must be the same
  length as `sd`.

- sd:

  A numeric vector of standard deviations to use in
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html). If a single value is
  provided, it will be recycled to the length of `x`. Otherwise, it must
  be the same length as `x`.

## Value

A numeric vector of the same length as `x` is returned.

## Details

### Historical methods

#### atlantisom

A lognormal distribution was used to sample using a mean of the natural
log of the expected value minus the input variance divided by two and
the input standard deviation. Where the input variance was calculated as
the natural log of the coefficient of variation squared plus one.

#### IP4EBFM

A normal distribution was used to sample the error using a mean of zero
and a standard deviation provided by the input. This error was then
exponentiated but not bias corrected before multiplying it by the
observation.

#### ss3sim

A normal distribution was used to sample the error using a mean of zero
and a standard deviation provided by the input. This error was then bias
corrected and exponentiated before multiplying it by the observation.

## Examples

``` r
# Note that setting the seed on a vectorized implementation of this function
# will not lead to the same sample being returned for each element of x
# because a single random number is generated for the entire vector.
set.seed(123)
sample_lognormal(1:10, sd = 1)
#>  [1]  0.3462911  0.9636469  8.6479353  2.6033596  3.4512126 20.2230786
#>  [7]  6.7316909  1.3694104  2.7466218  3.8842236
set.seed(123)
sample_lognormal(1, sd = 1)
#> [1] 0.3462911
sample_lognormal(2, sd = 1)
#> [1] 0.9636469

# You can also use the function with a vector of standard deviations
set.seed(123)
sample_lognormal(1:10, sd = 1:10)
#>  [1] 3.462911e-01 1.708093e-01 3.577761e+00 1.779057e-03 3.556589e-05
#>  [6] 2.690978e-03 4.037354e-09 4.077497e-18 4.793452e-20 2.237642e-23
```
