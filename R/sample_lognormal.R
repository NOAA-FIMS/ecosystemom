#' Sample using a lognormal distribution
#'
#' Sample from a standard normal in log space and apply this
#' exponentiated observation error to the observations, i.e, `x`, after the
#' simulated error values are bias corrected.
#'
#' @param x A numeric vector of observations to sample from. These observations
#'   should be on the normal scale. If a single value is provided, it will be
#'   recycled to the length of `sd`. Otherwise, it must be the same length as
#'   `sd`.
#' @param sd A numeric vector of standard deviations to use in [rnorm()]. If a
#'   single value is provided, it will be recycled to the length of `x`.
#'   Otherwise, it must be the same length as `x`.
#'
#' @details
#' ## Historical methods
#'
#' ### {atlantisom}
#'
#' A lognormal distribution was used to sample using a mean of the natural log
#' of the expected value minus the input variance divided by two and the input
#' standard deviation. Where the input variance was calculated as the natural
#' log of the coefficient of variation squared plus one.
#'
#' ### IP4EBFM
#'
#' A normal distribution was used to sample the error using a mean of zero and
#' a standard deviation provided by the input. This error was then
#' exponentiated but not bias corrected before multiplying it by the
#' observation.
#'
#' ### {ss3sim}
#'
#' A normal distribution was used to sample the error using a mean of zero and
#' a standard deviation provided by the input. This error was then bias
#' corrected and exponentiated before multiplying it by the observation.
#'
#' @examples
#' # Note that setting the seed on a vectorized implementation of this function
#' # will not lead to the same sample being returned for each element of x
#' # because a single random number is generated for the entire vector.
#' set.seed(123)
#' sample_lognormal(1:10, sd = 1)
#' set.seed(123)
#' sample_lognormal(1, sd = 1)
#' sample_lognormal(2, sd = 1)
#'
#' # You can also use the function with a vector of standard deviations
#' set.seed(123)
#' sample_lognormal(1:10, sd = 1:10)
#'
#' @return
#' A numeric vector of the same length as `x` is returned.
#' @export
sample_lognormal <- function(x, sd = 1) {
  # Input Validation ----
  # Check if x and sd are numeric
  if (any(!is.numeric(x)) || any(!is.numeric(sd))) {
    cli::cli_abort(c(
      "x" = "Both {.var x} and {.var sd} need to be numeric",
      "i" = "{.var x} is {.cls {class(x)}}",
      "i" = "{.var sd} is {.cls {class(sd)}}"
    ))
  }
  # Check if x and sd are of the same length
  if (length(x) != 1 && length(sd) != 1 && length(x) != length(sd)) {
    cli::cli_abort(c(
      "x" = "Length of {.var x} and {.var sd} must be the same",
      "i" = "{.var x} has length {length(x)}",
      "i" = "{.var sd} has length {length(sd)}"
    ))
  }
  # Check that all sd values are positive
  if (any(sd < 0)) {
    cli::cli_abort(c(
      "x" = "All values of {.var sd} must be positive",
      "i" = "{.var sd} has values {sd[sd < 0]}"
    ))
  }

  # Repeat x to same length as sd but you do not have to do the opposite because
  # the functions are properly vectorized if sd is length 1
  if (length(x) == 1 && length(sd) > 1) {
    x <- rep(x, length(sd))
  }

  # Sampling Process ----
  sampled_error <- stats::rnorm(n = length(x), mean = 0, sd = sd)
  bias_correction <- sd^2 / 2
  x * exp(sampled_error - bias_correction)
}
