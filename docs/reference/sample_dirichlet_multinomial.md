# Sample using a Dirichlet-multinomial distribution

Sample from a Dirichlet-multinomial distribution when you believe that
overdispersion should be present in the resulting data relative to
sampling from a multinomial distribution. Where, a Dirichlet-multinomial
distribution is typically used for compositional data when fish of
certain age groups are thought to aggregate in a way that is not uniform
across the age bins.

## Usage

``` r
sample_dirichlet_multinomial(x, sample_size)
```

## Arguments

- x:

  A numeric vector of positive concentration parameters for the
  Dirichlet distribution to sample from. These concentration parameters
  can be thought of as pseudocounts. Smaller sums of this vector will
  lead to more overdispersion in the resulting samples, i.e., as the sum
  reaches infinity the samples are thought to have arrived from a
  multinomial distribution rather than the Dirichlet-multinomial.

- sample_size:

  A single, positive integer specifying the total number of trials (n)
  for each multinomial draw. For example, if `sample_size = 20` it is
  assumed that you want 20 fish in your returned sample partitioned into
  bins. There is no default.

## Value

A numeric vector of one sample from the Dirichlet-multinomial
distribution with one entry per category. The sum of counts in each row
will equal the input `sample_size`.

## Examples

``` r
# Basic usage
sample_dirichlet_multinomial(x = c(1, 2, 3), sample_size = 10)
#> [1] 4 5 1

# More dispersed data
# (smaller x values also known as alpha or concentration parameters)
# Note: The total sum of alpha values influences the dispersion.
# Smaller sum -> more dispersion.
# Larger sum -> less dispersion (closer to multinomial).
sample_dirichlet_multinomial(
  x = c(0.1, 0.1, 0.1), sample_size = 20
)
#> [1] 11  9  0
```
