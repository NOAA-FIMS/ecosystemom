#' Sample using a gamma distribution
#'
#' Sample from a gamma distribution using the expected value in `x` and the
#' standard deviation in `sd`. The gamma shape and scale parameters are
#' derived from the input mean and standard deviation.
#'
#' @param x A numeric vector of observations to sample from. If a single value
#'   is provided, it will be recycled to the length of `sd`. Otherwise, it must
#'   be the same length as `sd`.
#' @param sd A numeric vector of standard deviations to use in [rgamma()]. If a
#'   single value is provided, it will be recycled to the length of `x`.
#'   Otherwise, it must be the same length as `x`.
#'
#' @details
#' The gamma distribution is parameterized by shape and scale. Given an
#' expected value `x` and a standard deviation `sd`, the shape parameter is
#' calculated as `x^2 / sd^2` and the scale parameter as `sd^2 / x`.
#'
#' @examples
#' set.seed(123)
#' sample_gamma(1, sd = 1)
#' set.seed(123)
#' sample_gamma(1, sd = c(0, 1))
#' sample_gamma(1:5, sd = 1)
#'
#' @return
#' A numeric vector of the same length as `x` is returned.
#' @export
sample_gamma <- function(x, sd = 1) {
  # Input Validation ----
  if (any(!is.numeric(x)) || any(!is.numeric(sd))) {
    cli::cli_abort(c(
      "x" = "Both {.var x} and {.var sd} need to be numeric",
      "i" = "{.var x} is {.cls {class(x)}}",
      "i" = "{.var sd} is {.cls {class(sd)}}"
    ))
  }

  if (length(x) != 1 && length(sd) != 1 && length(x) != length(sd)) {
    cli::cli_abort(c(
      "x" = "Length of {.var x} and {.var sd} must be the same",
      "i" = "{.var x} has length {length(x)}",
      "i" = "{.var sd} has length {length(sd)}"
    ))
  }

  if (any(sd < 0)) {
    cli::cli_abort(c(
      "x" = "All values of {.var sd} must be positive",
      "i" = "{.var sd} has values {sd[sd < 0]}"
    ))
  }

  if (any(x < 0)) {
    cli::cli_abort(c(
      "x" = "All values of {.var x} must be non-negative",
      "i" = "{.var x} has values {x[x < 0]}"
    ))
  }

  if (length(x) == 1 && length(sd) > 1) {
    x <- rep(x, length(sd))
  }

  if (any(x == 0 & sd > 0)) {
    cli::cli_abort(c(
      "x" = "Values of {.var x} must be positive when {.var sd} is greater than
      zero",
      "i" = "{.var x} has values {x[x == 0 & sd > 0]}"
    ))
  }

  result <- x
  non_degenerate <- sd != 0
  if (any(non_degenerate)) {
    shape <- x[non_degenerate]^2 / sd[non_degenerate]^2
    scale <- sd[non_degenerate]^2 / x[non_degenerate]
    result[non_degenerate] <- stats::rgamma(
      n = sum(non_degenerate),
      shape = shape,
      scale = scale
    )
  }

  result
}
