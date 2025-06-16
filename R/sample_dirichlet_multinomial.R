#' Sample using a Dirichlet-multinomial distribution
#'
#' Sample from a Dirichlet-multinomial distribution when you believe that
#' overdispersion should be present in the resulting data relative to sampling
#' from a multinomial distribution. Where, a Dirichlet-multinomial distribution
#' is typically used for compositional data when fish of certain age groups are
#' thought to aggregate in a way that is not uniform across the age bins.
#'
#' @param x A numeric vector of positive concentration parameters for the
#'   Dirichlet distribution to sample from. These concentration parameters can
#'   be thought of as pseudocounts. Smaller sums of this vector will lead to
#'   more overdispersion in the resulting samples, i.e., as the sum reaches
#'   infinity the samples are thought to have arrived from a multinomial
#'   distribution rather than the Dirichlet-multinomial.
#' @param sample_size A single, positive integer specifying the total number of
#'   trials (n) for each multinomial draw. For example, if `sample_size = 20`
#'   it is assumed that you want 20 fish in your returned sample partitioned
#'   into bins. There is no default.
#'
#' @return
#' A numeric vector of one sample from the Dirichlet-multinomial distribution
#' with one entry per category. The sum of counts in each row will equal the
#' input `sample_size`.
#' @export
#' @examples
#' # Basic usage
#' sample_dirichlet_multinomial(x = c(1, 2, 3), sample_size = 10)
#'
#' # More dispersed data
#' # (smaller x values also known as alpha or concentration parameters)
#' # Note: The total sum of alpha values influences the dispersion.
#' # Smaller sum -> more dispersion.
#' # Larger sum -> less dispersion (closer to multinomial).
#' sample_dirichlet_multinomial(
#'   x = c(0.1, 0.1, 0.1), sample_size = 20
#' )
sample_dirichlet_multinomial <- function(x, sample_size) {
  # Input Validation ----
  # Check that x is a numeric vector with all values greater than or equal to 0
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
      x = "{.var sample_size} must be a single positive integer.",
      i = "{.var sample_size} is {sample_size}."
    ))
  }

  # Number of categories
  K <- length(x)

  # Sampling Process ----

  # 1. Sample K probability vectors (p) from a Dirichlet distribution
  # rdirichlet is often found in packages like 'gtools' or 'MCMCpack'
  # gtools::rdirichlet(n = 1, alpha = x)
  # Manually to sample from Dirichlet(alpha)
  # draw K independent Gamma(alpha_i, 1) variables and normalize by their sum
  gamma_samples <- stats::rgamma(K, shape = x, rate = 1)
  dirichlet_probs <- gamma_samples / sum(gamma_samples)

  # 2. Sample counts from a Multinomial distribution
  sample_multinomial(dirichlet_probs, sample_size)
}
