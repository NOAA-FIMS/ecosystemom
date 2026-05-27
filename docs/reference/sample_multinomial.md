# Sample using a multinomial distribution

Sample from a multinomial distribution given a vector of counts or
proportions and a desired number of specimens in the returned sample.

## Usage

``` r
sample_multinomial(x, sample_size)
```

## Arguments

- x:

  A numeric vector of counts per bin. The values do not need to be
  integer values or sum to one because they will be converted to
  proportions using
  [`proportions()`](https://rdrr.io/r/base/proportions.html) prior to
  being passed to the `prop` argument of
  [`rmultinom()`](https://rdrr.io/r/stats/Multinom.html).

- sample_size:

  A single, positive integer specifying the number of samples you want
  to partition into bins. For example, if `sample_size = 20` it is
  assumed that you want 20 fish in your returned sample partitioned into
  bins. There is no default.

## Value

A vector the same length as `x` is returned, where the sum of the vector
equals the input to `sample_size`.

## Details

Sampling from a multinomial distribution is often used when composition
data are simulated. For example, in your operating model you will know
how many fish there are per age bin. The number of fish per age bin is
transformed into proportions per age bin and the user can specify the
final sample size of fish that they want in the returned vector.

The number of fish per age bin often include fractional fish in a
simulation because numbers of fish will be calculated from biomass and
mean weight at age. Thus, the input vector to `x` can be integers,
between 0 and 1, or real numbers. Everything will work but the result
will always be integers representing the number of fish per bin.

## Examples

``` r
# Sample from a multinomial distribution with 20 fish and 3 age bins
set.seed(123)
sample_multinomial(c(10, 5, 5), sample_size = 20)
#> [1] 9 7 4
```
