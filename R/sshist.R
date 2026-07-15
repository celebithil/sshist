#' Optimal Histogram Binning (Shimazaki-Shinomoto Method)
#'
#' Computes the optimal bin width and number of bins using the
#' Shimazaki-Shinomoto (2007) method. This implementation performs an
#' exhaustive search over candidate bin counts, exactly matching the
#' original Python/MATLAB reference algorithms.
#'
#' @param x   A numeric vector of data. \code{NA} values are silently removed.
#' @param n_max Integer or \code{NULL}. Maximum number of bins to consider.
#'   When \code{NULL} (default) the limit is set automatically as
#'   \code{min(500, floor(range / (2 * min_resolution)), n_samples)}.
#' @param sn  Integer. Number of histogram shifts used in the shift-average
#'   (default \code{30}).
#' @param ncores Integer. Number of OpenMP threads to use. Defaults to 1 for CRAN compliance.
#'
#' @return An object of class \code{"sshist"} (a named list) containing:
#' \describe{
#'   \item{\code{opt_n}}{Optimal number of bins (integer).}
#'   \item{\code{opt_d}}{Optimal bin width (\eqn{= \mathrm{range} / N_{\mathrm{opt}}}).}
#'   \item{\code{edges}}{Numeric vector of \eqn{N_{\mathrm{opt}} + 1} break
#'     points for the optimal histogram.}
#'   \item{\code{data}}{Cleaned (NA-removed) input data.}
#' }
#'
#' @references
#' Shimazaki, H. and Shinomoto, S. (2007). A method for selecting the bin size
#' of a time histogram. \emph{Neural Computation}, \strong{19}(6), 1503–1527.
#'
#' @export
sshist <- function(x, n_max = NULL, sn = 30, ncores = getOption("sshist.ncores", 1L)) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.numeric(x)) stop("'x' must be a numeric vector.")
  x <- as.numeric(na.omit(x))
  n_samples <- length(x)
  if (n_samples < 2L) stop("At least 2 non-missing data points are required.")

  x_min <- min(x)
  x_max <- max(x)
  range_x <- x_max - x_min

  # ── Resolution guard (anti-comb effect) ───────────────────────────────────
  x_unique <- sort(unique(x))
  if (length(x_unique) < 2L) stop("Data are constant (all values identical).")
  dx_min <- min(diff(x_unique))

  # Sampling Resolution limit (Protection against comb effect)
  # N_max = Range / (2 * Min_Resolution)
  # This matches the reference Python logic: min(floor(Range / (2 * dx_min)), max(N))
  n_limit_resolution <- floor(range_x / (2 * dx_min))

  # ── N bounds ──────────────────────────────────────────────────────────────
  N_MIN <- 2L

  N_MAX <- if (is.null(n_max)) {
    # Default cap: smallest of (500, Resolution Limit, Sample Size)
    min(500L, n_limit_resolution, n_samples)
  } else {
    if (!is.numeric(n_max) || n_max < 1)
      stop("'n_max' must be a positive integer or NULL.")
    # User override, but MUST still be bounded by resolution limit and sample size
    min(as.integer(n_max), n_limit_resolution, n_samples)
  }

  if (N_MAX < N_MIN) N_MAX <- N_MIN

  # ── Exhaustive Search over N (matches Python/MATLAB references) ───────────
  N_vec <- N_MIN:N_MAX
  D_vec <- range_x / N_vec

  # sshist_cost_cpp processes the vector of N values and returns the mean costs
  C_vec <- sshist_cost_cpp(x, N_vec, sn, x_min, x_max, ncores)

  # ── Find Optimum ──────────────────────────────────────────────────────────
  idx <- which.min(C_vec)
  opt_n <- N_vec[idx]
  opt_d <- D_vec[idx]
  edges <- seq(x_min, x_max, length.out = opt_n + 1L)

  # ── Return ────────────────────────────────────────────────────────────────
  structure(
    list(opt_n = opt_n,
         opt_d = opt_d,
         edges = edges,
         data  = x
    ),
    class = "sshist"
  )
}

#' Optimal 2D Histogram Binning (Shimazaki-Shinomoto Method)
#'
#' Computes the optimal number of bins for a 2-dimensional histogram using the
#' Shimazaki-Shinomoto (2007) cost function.
#'
#' @param x Numeric vector (X coordinates) or a 2-column matrix/data.frame.
#' @param y Numeric vector (Y coordinates). Ignored if \code{x} is a matrix.
#' @param n_min Integer. Minimum number of bins per axis (default \code{2}).
#' @param n_max Integer or \code{NULL}. Maximum number of bins per axis
#'   (default \code{200}). Automatically clamped to the data resolution limit
#'   and the sample size.
#'
#' @return An object of class \code{"sshist_2d"} containing:
#' \describe{
#'   \item{\code{opt_nx}, \code{opt_ny}}{Optimal bin counts.}
#'   \item{\code{opt_dx}, \code{opt_dy}}{Optimal bin widths.}
#'   \item{\code{data}}{Cleaned input data.}
#' }
#'
#' @references
#' Shimazaki, H. and Shinomoto, S. (2007). A method for selecting the bin size
#' of a time histogram. \emph{Neural Computation}, \strong{19}(6), 1503-1527.
#' \doi{10.1162/neco.2007.19.6.1503}
#'
#' @importFrom stats na.omit optimize
#' @export
#'
#' @examples
#' set.seed(42)
#' x <- rnorm(500); y <- rnorm(500)
#'
#' res <- sshist_2d(x, y)
#' plot(res)
#'
#' # ggplot2
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(data.frame(x = x, y = y), aes(x, y)) +
#'     geom_bin2d(bins = c(res$opt_nx, res$opt_ny)) +
#'     scale_fill_viridis_c() +
#'     ggtitle(sprintf("Optimal 2D Bins: %dx%d", res$opt_nx, res$opt_ny)) +
#'     theme_minimal()
#' }
sshist_2d <- function(x, y = NULL, n_min = 2L, n_max = 200L) {

  # ── 1. Data preparation ───────────────────────────────────────────────────
  if (is.null(y)) {
    if (!is.matrix(x) && !is.data.frame(x))
      stop("'x' must be a 2-column matrix or data.frame when 'y' is NULL.")
    if (NCOL(x) != 2L)
      stop("'x' must have exactly 2 columns when 'y' is NULL.")
    y <- as.numeric(x[, 2L])
    x <- as.numeric(x[, 1L])
  } else {
    x <- as.numeric(x)
    y <- as.numeric(y)
  }

  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]; y <- y[valid]
  n_samples <- length(x)
  if (n_samples < 2L) stop("At least 2 complete (non-NA) observations are required.")

  x_min <- min(x); x_max <- max(x); range_x <- x_max - x_min
  y_min <- min(y); y_max <- max(y); range_y <- y_max - y_min

  if (range_x == 0) stop("X data are constant.")
  if (range_y == 0) stop("Y data are constant.")

  # ── 2. Resolution guard ───────────────────────────────────────────────────
  min_gap <- function(vals) {
    u <- sort(unique(vals))
    if (length(u) < 2L) return(Inf)
    d <- min(diff(u))
    # Protect against machine epsilon issues
    if (d < .Machine$double.eps * 100) Inf else d
  }

  dx_min <- min_gap(x); dy_min <- min_gap(y)

  limit_nx <- if (is.finite(dx_min)) floor(range_x / (2 * dx_min)) else n_max
  limit_ny <- if (is.finite(dy_min)) floor(range_y / (2 * dy_min)) else n_max

  # Cap: resolution limit, sample size, and user-supplied n_max
  N_MAX_x <- max(n_min, min(n_max, limit_nx, n_samples))
  N_MAX_y <- max(n_min, min(n_max, limit_ny, n_samples))

  n_min <- as.integer(n_min)

  # ── 3. Core cost for a single (nx, ny) pair ───────────────────────────────
  # Optimised vs original:
  #   - k_mean = n_samples / (nx * ny)  [no tabulate needed for mean]
  #   - k_var  = sum(k^2)/M - k_mean^2  [numerically equivalent but faster]
  compute_cost <- function(nx, ny) {
    nx <- as.integer(nx); ny <- as.integer(ny)
    dx <- range_x / nx;  dy <- range_y / ny

    bx <- findInterval(x,
                       seq(x_min, x_max, length.out = nx + 1L),
                       rightmost.closed = TRUE)
    by <- findInterval(y,
                       seq(y_min, y_max, length.out = ny + 1L),
                       rightmost.closed = TRUE)

    # counts[i, j] = number of points in X-bin i, Y-bin j
    counts <- tabulate((bx - 1L) * ny + by, nbins = nx * ny)

    M      <- nx * ny
    k_mean <- n_samples / M                       # exact, no floating-point iteration
    k_var  <- sum(counts^2L) / M - k_mean^2       # equivalent to biased variance (divide by N, not N-1) as per Shimazaki & Shinomoto

    # Cost Function: C = (2*mean - var) / Area^2
    (2 * k_mean - k_var) / (dx * dy)^2
  }

  # ── 4. Grid search ───────────────────────────────────────────────────────
  nx_vector <- n_min:N_MAX_x
  ny_vector <- n_min:N_MAX_y

  # KEY OPTIMISATION: Y bin indices do not depend on nx.
  # Pre-compute them once for all ny values BEFORE the outer (nx) loop.
  # Savings: length(nx_vector) * length(ny_vector) → length(ny_vector) findInterval calls.
  bin_idx_y_list <- lapply(ny_vector, function(ny) {
    findInterval(y, seq(y_min, y_max, length.out = ny + 1L),
                 rightmost.closed = TRUE)
  })

  cost_matrix <- matrix(NA_real_, nrow = length(nx_vector),
                        ncol = length(ny_vector))

  for (i in seq_along(nx_vector)) {
    nx <- nx_vector[i]
    dx <- range_x / nx
    bx <- findInterval(x,
                       seq(x_min, x_max, length.out = nx + 1L),
                       rightmost.closed = TRUE)

    for (j in seq_along(ny_vector)) {
      ny  <- ny_vector[j]
      dy  <- range_y / ny
      by  <- bin_idx_y_list[[j]]          # pre-computed — no findInterval here

      counts <- tabulate((bx - 1L) * ny + by, nbins = nx * ny)
      M      <- nx * ny
      k_mean <- n_samples / M
      k_var  <- sum(counts^2L) / M - k_mean^2

      cost_matrix[i, j] <- (2 * k_mean - k_var) / (dx * dy)^2
    }
  }

  # Use arrayInd for safety (which.min on matrix returns linear index)
  best <- arrayInd(which.min(cost_matrix), dim(cost_matrix))
  opt_nx   <- nx_vector[best[1L]]
  opt_ny   <- ny_vector[best[2L]]
  min_cost <- cost_matrix[best]

  res <- list(
    opt_nx      = opt_nx,
    opt_ny      = opt_ny,
    opt_dx      = range_x / opt_nx,
    opt_dy      = range_y / opt_ny,
    data        = list(x = x, y = y)
  )

  class(res) <- "sshist_2d"
  res
}

# --- S3 Methods ---

#' Plot method for sshist objects
#'
#' Produces single-panel histogram with a jittered-data strip drawn BELOW the
#' x-axis (in a reserved negative-y band), so that the jittered points do not
#' overlap with the histogram bars.
#'
#' @export
#' @param x An object of class \code{"sshist"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{hist}}.
#' @return No return value; called for its side effect of producing a plot.
plot.sshist <- function(x, ...) {
  dots <- list(...)
  if (!"xlab" %in% names(dots)) dots$xlab <- "Data"
  if (!"main" %in% names(dots)) dots$main <- paste0("Optimal Histogram (N = ", x$opt_n, ")")

  # Pre-compute histogram to obtain y_max (needed to set a y-axis limit
  # that reserves a bottom strip below y = 0 for the jittered points).
  h <- hist(x$data, breaks = x$edges, plot = FALSE)
  y_max <- max(h$density)

  # Reserve a bottom strip (~10% of y_max below zero) for the data strip
  # (rug + jittered points), so they never overlap the histogram bars.
  if (!"ylim" %in% names(dots)) {
    dots$ylim <- c(-y_max * 0.10, y_max * 1.05)
  }

  # Suppress the default y-axis: the negative part of ylim is reserved for
  # the data strip and should NOT show negative density ticks.
  dots$yaxt <- "n"

  dots$x <- x$data
  dots$breaks <- x$edges
  dots$freq <- FALSE
  dots$col <- "lightblue"
  dots$border <- "white"
  do.call(hist, dots)

  # Redraw a clean y-axis with only non-negative ticks
  usr <- par("usr")
  y_ticks <- pretty(c(0, usr[4]))
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= usr[4]]
  axis(2, at = y_ticks, las = 1)

  # Subtle separator line at the x-axis (y = 0) — visually delimits the
  # data strip from the histogram area.
  abline(h = 0, col = "gray70", lwd = 0.5)

  # Rug at the bottom of the reserved strip (uses usr[3] as the baseline).
  rug(x$data, col = adjustcolor("gray40", alpha.f = 0.5))

  # Jittered points ABOVE the rug, but BELOW y = 0.
  # Layout (within the bottom strip of height 0.10 * y_max):
  #   - rug ticks:    usr[3]            .. usr[3] + 3% * y_range   (default)
  #   - jitter band:  usr[3] + 4%*range .. usr[3] + 8%*range       (no overlap with rug)
  y_range <- usr[4] - usr[3]
  y_base  <- rep(usr[3] + y_range * 0.06, length(x$data))

  points(
    x = x$data,
    y = jitter(y_base, amount = y_range * 0.02),
    pch = 16,
    cex = 0.6,
    col = adjustcolor("darkred", alpha.f = 0.4)
  )
}

#' Print method for sshist objects
#' @export
#' @param x An object of class \code{"sshist"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @return Returns \code{x} invisibly.
print.sshist <- function(x, ...) {
  cat("Shimazaki-Shinomoto Histogram Optimization\n")
  cat("------------------------------------------\n")
  cat("Optimal Bins (N):", x$opt_n, "\n")
  cat("Bin Width (D):   ", format(x$opt_d,  digits = 4), "\n")
  invisible(x)
}

#' Plot method for sshist_2d objects
#'
#' Draws the optimal 2D histogram as a heatmap with proper data-coordinate axes.
#'
#' @export
#' @param x An object of class \code{"sshist_2d"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{image}}.
#' @return No return value; called for its side effect of producing a plot.
plot.sshist_2d <- function(x, ...) {
  # Extract data
  nx     <- x$opt_nx
  ny     <- x$opt_ny
  data_x <- x$data$x
  data_y <- x$data$y

  # Bin boundaries and midpoints (for axis tick positions)
  x_breaks <- seq(min(data_x), max(data_x), length.out = nx + 1L)
  y_breaks <- seq(min(data_y), max(data_y), length.out = ny + 1L)
  x_mids   <- (x_breaks[-1L] + x_breaks[-(nx + 1L)]) / 2
  y_mids   <- (y_breaks[-1L] + y_breaks[-(ny + 1L)]) / 2

  # cut() matches stat_bin2d: (a,b] intervals, first bin [a,b]
  bx     <- as.integer(cut(data_x, x_breaks, include.lowest = TRUE))
  by_idx <- as.integer(cut(data_y, y_breaks, include.lowest = TRUE))

  # <-- critical: index is row-major (Y varies fastest), R matrix is column-major
  counts <- matrix(tabulate((bx - 1L) * ny + by_idx, nbins = nx * ny),
                   nrow = nx, ncol = ny, byrow = TRUE)

  # Zero cells -> NA: image() renders them as background (transparent),
  # matching ggplot2's geom_bin2d which does not draw zero-count tiles.
  counts[counts == 0L] <- NA_integer_

  col <- hcl.colors(64, "Inferno")

  # Assemble arguments for image()
  dots <- list(...)
  if (!"xlab" %in% names(dots)) dots$xlab <- "X"
  if (!"ylab" %in% names(dots)) dots$ylab <- "Y"
  if (!"main" %in% names(dots)) {
    dots$main <- sprintf("Shimazaki-Shinomoto 2D Histogram (%d \u00d7 %d bins)", nx, ny)
  }
  dots$x <- x_mids
  dots$y <- y_mids
  dots$z <- counts
  dots$col <- col

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(bg = "gray92")

  # Main plot (left side)
  par(fig = c(0, 0.88, 0, 1), mar = c(4, 4, 3, 1))
  do.call(image, dots)

  # Cell borders and outline
  abline(v = x_breaks, h = y_breaks, col = "white", lwd = 0.4)
  points(data_x, data_y, pch = 20, col = adjustcolor("cyan", alpha.f = 0.5), cex = 0.7)
  box()

  # Color scale (right side)
  par(fig = c(0.88, 1, 0.1, 0.9), mar = c(2, 0.5, 2, 2.5), new = TRUE)
  plot.new()
  bar <- seq(min(counts, na.rm = TRUE),
             max(counts, na.rm = TRUE),
             length.out = length(col) + 1L)

  # <- key fix for scale alignment
  par(yaxs = "i")
  plot.window(xlim = c(0, 1), ylim = range(bar))

  usr <- par("usr")
  # Grey panel background (drawn before borders so borders sit on top)
  rect(usr[1], usr[3], usr[2], usr[4], col = "white", border = NA)

  image(x = c(0, 1), y = bar,
        z = matrix(seq_along(col), nrow = 1),
        col = col, add = TRUE)

  axis(4, at = pretty(range(counts, na.rm = TRUE), n = 5L),
       las = 1L, tcl = -0.3, mgp = c(0, 0.4, 0))
  box()
}

#' Print method for sshist_2d objects
#' @export
#' @param x An object of class sshist.
#' @param ... Additional arguments passed to print.
#' @return Returns the input object \code{x} invisibly. The method is called for its
#'   side effect of printing a summary of the 2D Shimazaki-Shinomoto histogram
#'   optimization results, including the optimal number of bins for both X and Y
#'   dimensions, bin widths, and minimum cost value.
print.sshist_2d <- function(x, ...) {
  cat("Shimazaki-Shinomoto 2D Histogram Optimization\n")
  cat("---------------------------------------------\n")
  cat("Optimal Bins X:  ", x$opt_nx, "\n")
  cat("Optimal Bins Y:  ", x$opt_ny, "\n")
  cat("Bin Width X:     ", format(x$opt_dx, digits=4), "\n")
  cat("Bin Width Y:     ", format(x$opt_dy, digits=4), "\n")
  invisible(x)
}
