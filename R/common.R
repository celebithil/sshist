#' Check whether the bootstrap package is available
#' @keywords internal
#' @noRd
pkg_has_boot <- function() {
  requireNamespace("boot", quietly = TRUE)
}

#' Internal helper: Fast convolution via FFT for Gaussian kernel
#' @keywords internal
#' @noRd
fftkernel_1d <- function(x, w) {
  L <- length(x)
  Lmax <- max(1, L + 3 * w)
  n <- 2^ceiling(log2(Lmax))
  X <- stats::fft(c(x, numeric(n - L)))

  # Symmetric frequency vector aligned with R's fft() structure
  f <- pmin((0:(n - 1)) / n, 1 - (0:(n - 1)) / n)

  K <- exp(-0.5 * (w * 2 * pi * f)^2)
  y <- Re(stats::fft(X * K, inverse = TRUE)) / n
  return(y[1:L])
}
