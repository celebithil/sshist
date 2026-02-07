#' Optimal Histogram Binning (Shimazaki-Shinomoto Method)
#'
#' Computes the optimal bin width and number of bins using the Shimazaki-Shinomoto
#' method. This implementation uses Rcpp for high performance and includes
#' shift-averaging to remove edge-position bias.
#'
#' @param x A numeric vector of data. Missing values (NA) will be removed.
#' @param n_max An integer specifying the maximum number of bins to test.
#'   If \code{NULL} (default), it is automatically determined based on data resolution
#'   and sample size to prevent overfitting.
#' @param sn Integer. The number of shifts to use for averaging (default is 30).
#'
#' @return An object of class \code{"sshist"} containing:
#' \describe{
#'   \item{opt_n}{The optimal number of bins.}
#'   \item{opt_d}{The optimal bin width.}
#'   \item{edges}{The sequence of break points for the optimal histogram.}
#'   \item{cost}{A numeric vector of the cost function values.}
#'   \item{n_tested}{The vector of N values tested.}
#'   \item{data}{The original data (cleaned).}
#' }
#'
#' @references
#' Shimazaki, H. and Shinomoto, S., 2007. A method for selecting the bin size of a time histogram.
#' \emph{Neural Computation}, 19(6), pp.1503-1527.
#'
#' @useDynLib sshist, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom graphics hist rug points lines par grid image box
#' @importFrom stats na.omit
#' @importFrom grDevices terrain.colors hcl.colors
#' @export
#'
#' @examples
#' # 1. Basic usage with base graphics
#' data(faithful)
#' res <- sshist(faithful$waiting)
#' plot(res)
#'
#' # 2. Usage with ggplot2
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   df <- data.frame(waiting = faithful$waiting)
#'
#'   ggplot(df, aes(x = waiting)) +
#'     geom_histogram(breaks = res$edges, fill = "lightblue", color = "black") +
#'     ggtitle(paste("Optimal Shimazaki-Shinomoto Bins (N =", res$opt_n, ")")) +
#'     theme_minimal()
#' }
sshist <- function(x, n_max = NULL, sn = 30) {

  if (!is.numeric(x)) stop("Data 'x' must be numeric.")
  x <- as.numeric(na.omit(x))
  n_samples <- length(x)

  if (n_samples < 2) stop("Need at least 2 data points.")

  x_min <- min(x)
  x_max <- max(x)

  # --- 1. Resolution Check (Anti-Comb Effect) ---
  x_unique <- sort(unique(x))

  # Protect against constant data
  if (length(x_unique) < 2) stop("Data is constant.")

  dx_min <- min(abs(diff(x_unique)))

  # Sampling Resolution limit
  # As per FAQ: "Do not search the bin width smaller than the resolution."
  # N_max = Range / Min_Bin_Width
  n_limit_resolution <- floor((x_max - x_min) / dx_min)

  if (is.null(n_max)) {
    # Default cap: smallest of (500, Resolution Limit, Sample Size)
    n_stop <- min(500, n_limit_resolution, n_samples)
  } else {
    # User override, but still bounded by sample size (cannot have more bins than data points)
    n_stop <- min(n_max, n_samples)
  }

  if (n_stop < 2) n_stop <- 2

  n_vector <- 2:n_stop

  # --- 2. C++ Calculation ---
  costs <- sshist_cost_cpp(x, n_vector, sn, x_min, x_max)

  # --- 3. Result Construction ---
  idx_min <- which.min(costs)
  opt_n   <- n_vector[idx_min]
  opt_d   <- (x_max - x_min) / opt_n
  edges   <- seq(x_min, x_max, length.out = opt_n + 1)

  res <- list(
    opt_n    = opt_n,
    opt_d    = opt_d,
    edges    = edges,
    cost     = costs,
    n_tested = n_vector,
    data     = x
  )
  class(res) <- "sshist"
  return(res)
}

#' Optimal 2D Histogram Binning (Shimazaki-Shinomoto Method)
#'
#' Computes the optimal number of bins for a 2-dimensional histogram.
#' Includes checks for data resolution and biased variance calculation.
#'
#' @param x Numeric vector (coord X) or a 2-column matrix.
#' @param y Numeric vector (coord Y). Optional if x is a matrix.
#' @param n_min Integer. Minimum number of bins (default 2).
#' @param n_max Integer. Maximum number of bins (default 200).
#'              Algorithm will reduce this automatically if data resolution restricts it.
#' @return An object of class \code{"sshist_2d"} containing optimal parameters.
#' @export
#'
#' @examples
#' # 1. Basic usage with base graphics
#' set.seed(42)
#' x <- rnorm(500)
#' y <- rnorm(500)
#' res <- sshist_2d(x, y)
#' plot(res)
#'
#' # 2. Usage with ggplot2
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   df <- data.frame(x = x, y = y)
#'
#'   ggplot(df, aes(x = x, y = y)) +
#'     geom_bin2d(bins = c(res$opt_nx, res$opt_ny)) +
#'     scale_fill_viridis_c() +
#'     ggtitle(paste0("Optimal 2D Bins: ", res$opt_nx, "x", res$opt_ny)) +
#'     theme_minimal()
#' }
sshist_2d <- function(x, y = NULL, n_min = 2, n_max = 200) {

  # --- 1. Data Preparation ---
  if (is.null(y)) {
    if (NCOL(x) != 2) stop("x must be a 2-column matrix if y is not provided")
    y <- x[, 2]
    x <- x[, 1]
  }

  # Remove NAs
  valid_mask <- !is.na(x) & !is.na(y)
  x <- x[valid_mask]
  y <- y[valid_mask]

  if (length(x) < 2) stop("Need at least 2 data points.")

  x_min <- min(x); x_max <- max(x)
  y_min <- min(y); y_max <- max(y)

  # --- 2. Resolution Check ---

  # Helper to find minimum difference (standardized naming)
  get_resolution <- function(vals) {
    vals_unique <- sort(unique(vals))
    if (length(vals_unique) < 2) return(Inf)
    d_min <- min(abs(diff(vals_unique)))
    # Protect against machine epsilon issues
    if (d_min < .Machine$double.eps * 100) return(Inf)
    return(d_min)
  }

  dx_min <- get_resolution(x)
  dy_min <- get_resolution(y)

  # Calculate limits based on resolution (FAQ compliance)
  # N_max = Range / Min_Bin_Width
  limit_nx <- floor((x_max - x_min) / dx_min)
  limit_ny <- floor((y_max - y_min) / dy_min)

  # Adjust n_max if resolution is too coarse
  actual_n_max_x <- min(n_max, limit_nx)
  actual_n_max_y <- min(n_max, limit_ny)

  # Ensure we respect n_min, even if resolution suggests otherwise (fallback)
  if (actual_n_max_x < n_min) actual_n_max_x <- n_min
  if (actual_n_max_y < n_min) actual_n_max_y <- n_min

  nx_vector <- n_min:actual_n_max_x
  ny_vector <- n_min:actual_n_max_y

  # --- 3. Grid Search ---
  # Rows: X bins, Columns: Y bins
  cost_matrix <- matrix(NA, nrow = length(nx_vector), ncol = length(ny_vector))

  for (i in seq_along(nx_vector)) {
    nx <- nx_vector[i]
    dx <- (x_max - x_min) / nx

    # rightmost.closed = TRUE ensures max value is included
    breaks_x <- seq(x_min, x_max, length.out = nx + 1)
    bin_idx_x <- findInterval(x, breaks_x, rightmost.closed = TRUE)

    for (j in seq_along(ny_vector)) {
      ny <- ny_vector[j]
      dy <- (y_max - y_min) / ny

      breaks_y <- seq(y_min, y_max, length.out = ny + 1)
      bin_idx_y <- findInterval(y, breaks_y, rightmost.closed = TRUE)

      # Linearize 2D indices to 1D for fast counting
      linear_indices <- (bin_idx_x - 1) * ny + bin_idx_y

      # Count events in each bin
      counts <- tabulate(linear_indices, nbins = nx * ny)

      # Statistics: Mean (k) and Biased Variance (v)
      k_mean <- mean(counts)

      # Use biased variance (divide by N, not N-1) as per Shimazaki & Shinomoto
      k_var  <- sum((counts - k_mean)^2) / (nx * ny)

      # Cost Function: C = (2*mean - var) / Area^2
      area <- dx * dy
      cost_matrix[i, j] <- (2 * k_mean - k_var) / area^2
    }
  }

  # --- 4. Result Construction ---
  min_idx <- which(cost_matrix == min(cost_matrix), arr.ind = TRUE)
  best_i <- min_idx[1, 1]
  best_j <- min_idx[1, 2]

  opt_nx <- nx_vector[best_i]
  opt_ny <- ny_vector[best_j]

  res <- list(
    opt_nx      = opt_nx,
    opt_ny      = opt_ny,
    opt_dx      = (x_max - x_min) / opt_nx,
    opt_dy      = (y_max - y_min) / opt_ny,
    min_cost    = cost_matrix[best_i, best_j],
    cost_matrix = cost_matrix,
    nx_tested   = nx_vector,
    ny_tested   = ny_vector,
    data        = list(x = x, y = y)
  )
  class(res) <- "sshist_2d"
  return(res)
}

# --- S3 Methods ---

#' Plot method for sshist objects
#' @export
#' @param x An object of class sshist.
#' @param ... Additional arguments passed to plot.
plot.sshist <- function(x, ...) {
  old_par <- par(mfrow=c(1,2))
  on.exit(par(old_par))

  # Plot 1: Cost Function
  plot(x$n_tested, x$cost, type='l', col='blue', lwd=2,
       main="Cost Minimization", xlab="N bins", ylab="Cost")
  points(x$opt_n, min(x$cost), col='red', pch=19, cex=1.5)
  grid()

  # Plot 2: Histogram
  hist(x$data, breaks=x$edges, freq=FALSE,
       main=paste("Optimal Hist (N=", x$opt_n, ")"),
       col="lightblue", border="white", xlab="Data")
  rug(x$data)
}

#' Print method for sshist objects
#' @export
#' @param x An object of class sshist.
#' @param ... Additional arguments passed to print.
print.sshist <- function(x, ...) {
  cat("Shimazaki-Shinomoto Histogram Optimization\n")
  cat("------------------------------------------\n")
  cat("Optimal Bins (N):", x$opt_n, "\n")
  cat("Bin Width (D):   ", format(x$opt_d, digits=4), "\n")
  cat("Cost Minimum:    ", format(min(x$cost), digits=4), "\n")
}

#' Plot method for sshist_2d objects
#' @export
#' @param x An object of class sshist.
#' @param ... Additional arguments passed to plot.
plot.sshist_2d <- function(x, ...) {
  nx <- x$opt_nx
  ny <- x$opt_ny
  data_x <- x$data$x
  data_y <- x$data$y

  # Re-calculate bins for plotting
  x_cut <- cut(data_x, breaks = nx)
  y_cut <- cut(data_y, breaks = ny)
  h <- table(x_cut, y_cut)

  old_par <- par(mfrow = c(1, 2))
  on.exit(par(old_par))

  # Plot 1: Cost Matrix Heatmap
  image(x$nx_tested, x$ny_tested, x$cost_matrix,
        main = "Cost Function Landscape",
        xlab = "N bins (X)", ylab = "N bins (Y)",
        col = terrain.colors(16))
  points(x$opt_nx, x$opt_ny, pch = 19, col = "red")
  box()

  # Plot 2: Optimal 2D Histogram
  image(h, main = paste0("Optimal 2D Hist\nNx=", nx, ", Ny=", ny),
        col = terrain.colors(16),
        axes = FALSE)
  box()
}

#' Print method for sshist_2d objects
#' @export
#' @param x An object of class sshist.
#' @param ... Additional arguments passed to print.
print.sshist_2d <- function(x, ...) {
  cat("Shimazaki-Shinomoto 2D Histogram Optimization\n")
  cat("---------------------------------------------\n")
  cat("Optimal Bins X:  ", x$opt_nx, "\n")
  cat("Optimal Bins Y:  ", x$opt_ny, "\n")
  cat("Bin Width X:     ", format(x$opt_dx, digits=4), "\n")
  cat("Bin Width Y:     ", format(x$opt_dy, digits=4), "\n")
  cat("Cost Minimum:    ", format(x$min_cost, digits=4), "\n")
}
