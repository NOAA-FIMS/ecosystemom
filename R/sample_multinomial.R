#' Sample using a multinomial distribution
#'
#' Sample from a multinomial distribution given a vector of counts or
#' proportions and a desired number of specimens in the returned sample.
#'
#' @param x A numeric vector of counts per bin. The values do not need to be
#'   integer values or sum to one because they will be converted to proportions
#'   using [proportions()] prior to being passed to the `prop` argument of
#'   [rmultinom()].
#' @param sample_size A single, positive integer specifying the number of
#'   samples you want to partition into bins. For example, if `sample_size =
#'   20` it is assumed that you want 20 fish in your returned sample
#'   partitioned into bins. There is no default.
#'
#' @details
#' Sampling from a multinomial distribution is often used when composition data
#' are simulated. For example, in your operating model you will know how many
#' fish there are per age bin. The number of fish per age bin is transformed
#' into proportions per age bin and the user can specify the final sample size
#' of fish that they want in the returned vector.
#'
#' The number of fish per age bin often include fractional fish in a simulation
#' because numbers of fish will be calculated from biomass and mean weight at
#' age. Thus, the input vector to `x` can be integers, between 0 and 1, or real
#' numbers. Everything will work but the result will always be integers
#' representing the number of fish per bin.
#'
#' @examples
#' # Sample from a multinomial distribution with 20 fish and 3 age bins
#' set.seed(123)
#' sample_multinomial(c(10, 5, 5), sample_size = 20)
#'
#' @return
#' A vector the same length as `x` is returned, where the sum of the vector
#' equals the input to `sample_size`.
#' @export
sample_multinomial <- function(x, sample_size) {
  # Input Validation ----
  # Check that x is a numeric vector with all values greater than or equal to 0
  # Proportions of all zeros leads to NA and a non-informative error
  if (!is.numeric(x) ||
    !is.vector(x) ||
    any(x < 0) ||
    length(x) == 0 ||
    all(x <= 0)) {
    cli::cli_abort(c(
      x = "{.var x} must be a non-negative numeric vector with at least one
          value greater than zero.",
      i = "{.var x} is {.cls {class(x)}}.",
      i = "{.var x} is {x}."
    ))
  }
  # Check that sample_size is a positive integer
  if (length(sample_size) != 1 ||
    !is.numeric(sample_size) ||
    sample_size <= 0 ||
    sample_size != round(sample_size)
  ) {
    cli::cli_abort(c(
      x = "{.var sample_size} must be a single positive integer",
      i = "{.var sample_size} is {sample_size}"
    ))
  }

  # Sampling Process ----
  stats::rmultinom(
    n = 1,
    size = sample_size,
    prob = proportions(x)
  )[, 1]
}
