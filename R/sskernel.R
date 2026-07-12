#' @title Optimal 1D Kernel Density Estimation (Fixed Bandwidth)
#' @description Computes the optimal global bandwidth for 1D kernel density estimation
#' using the Shimazaki-Shinomoto method. Minimizes the expected Mean Integrated Squared Error (MISE).
#'
#' @param x Numeric vector of sample data. Missing values (NA) will be removed.
#' @param tin Optional numeric vector specifying the grid of evaluation points.
#' @param W Optional numeric vector of bandwidths to search. If provided, the grid search is skipped.
#' @param nbs Integer specifying the number of bootstrap samples for calculating confidence intervals.
#' @param ncores Integer specifying the number of CPU cores to use for bootstrap (default: 1).
#'
#' @return An object of class \code{"sskernel"} containing:
#' \describe{
#'   \item{x}{The evaluation points (same as \code{tin}).}
#'   \item{y}{The optimized kernel density estimate.}
#'   \item{optw}{The optimal global bandwidth.}
#'   \item{data}{The original evaluated data.}
#'   \item{confb95}{(If nbs > 0) A matrix with 5th and 95th percentile bootstrap confidence intervals.}
#'   \item{yb}{(If nbs > 0) A matrix of all bootstrap density samples.}
#' }
#' @export
sskernel <- function(x, tin = NULL, W = NULL, nbs = 0, ncores = getOption("sshist.ncores", 1L)) {
  x <- sort(stats::na.omit(x))

  # Automatically build the evaluation grid 'tin' if not provided.
  # Matches the Python reference: grid size = min(ceil(T / dt_samp), 1000),
  # where dt_samp is the smallest non-zero gap between sorted data values.
  # This ensures the R and Python implementations produce identical grids.
  if (is.null(tin)) {
    T_span <- max(x) - min(x)
    dx <- diff(x)
    dx <- dx[dx > 0]
    dt_samp <- if (length(dx) > 0) min(dx) else T_span / 1000
    tin <- seq(min(x), max(x), length.out = min(ceiling(T_span / dt_samp), 1000))
    t_eval <- tin
    x_valid <- x[x >= min(tin) & x <= max(tin)]
  } else {
    T_span <- max(tin) - min(tin)
    x_valid <- x[x >= min(tin) & x <= max(tin)]
    dx <- diff(sort(x_valid))
    dx <- dx[dx > 0]
    dt_samp <- if (length(dx) > 0) min(dx) else T_span / 1000
    if (dt_samp > min(diff(tin))) {
      t_eval <- seq(min(tin), max(tin), length.out = min(ceiling(T_span / dt_samp), 1000))
    } else {
      t_eval <- tin
    }
  }

  dt <- min(diff(t_eval))
  # Adjust histogram boundaries
  breaks <- c(t_eval - dt / 2, t_eval[length(t_eval)] + dt / 2)
  N <- length(x_valid)

  # Fine-grained histogram (used for FFT evaluation)
  # 'right = FALSE' strictly mimics Python's np.histogram half-open bins [a, b)
  y_hist <- graphics::hist(x_valid, breaks = breaks, right = FALSE, plot = FALSE)$counts / (N * dt)

  # --- COST FUNCTION WRAPPER (FFT ONLY) ---
  cost_function <- function(w) {
    yh <- fftkernel_1d(y_hist, w / dt)
    C  <- sum(yh^2) * dt - 2 * sum(yh * y_hist) * dt + 2 / (sqrt(2 * pi) * w * N)
    return(C * N^2)
  }

  # --- BANDWIDTH OPTIMIZATION (GRID EXPANSION) ---
  if (!is.null(W)) {
    # User-supplied grid: evaluate and pick minimum
    C_vals <- vapply(W, cost_function, numeric(1))
    optw <- W[which.min(C_vals)]
    W_grid <- W
  } else {
    # 1. Protection against singularity of duplicates (Silverman's Rule)
    # Calculate reference bandwidth to establish a statistical floor
    sd_x <- stats::sd(x_valid)
    iqr_x <- stats::IQR(x_valid)
    scale_x <- if (iqr_x > 0) min(sd_x, iqr_x / 1.34) else sd_x
    if (scale_x == 0) scale_x <- 1
    w_rot <- 0.9 * scale_x * N^(-0.2)

    # Hard floor: window cannot be smaller than a tenth of the normal or grid limits
    Wmin_limit <- max(2 * dt, w_rot / 10, dt_samp)
    Wmax_initial <- max(x) - min(x)

    # Protection for degenerate data
    if (Wmax_initial <= Wmin_limit) Wmax_initial <- Wmin_limit * 10

    # 2. Starting logarithmic grid of 40 points
    W_grid <- exp(seq(log(Wmin_limit), log(Wmax_initial), length.out = 40))
    C_vals <- vapply(W_grid, cost_function, numeric(1))

    # 3. Smart boundary expansion (Grid Auto-Expansion)
    max_expansions <- 10
    for (expansion in seq_len(max_expansions)) {
      min_idx <- which.min(C_vals)
      n_w <- length(W_grid)

      if (min_idx == 1) {
        # Minimum on the LEFT boundary (moving towards smaller windows)
        if (W_grid[1] <= Wmin_limit * 1.01) break # Protection against physical zero

        log_step <- log(W_grid[2]) - log(W_grid[1])
        w_new <- exp(seq(log(W_grid[1]) - 5 * log_step, log(W_grid[1]) - log_step, length.out = 5))
        w_new <- w_new[w_new >= Wmin_limit]

        if (length(w_new) == 0) break

        # Calculate only new points and prepend to the grid
        C_new <- vapply(w_new, cost_function, numeric(1))
        W_grid <- c(w_new, W_grid)
        C_vals <- c(C_new, C_vals)
      } else if (min_idx == n_w) {
        # Minimum on the RIGHT boundary (moving towards wider windows)
        log_step <- log(W_grid[n_w]) - log(W_grid[n_w - 1])
        w_new <- exp(seq(log(W_grid[n_w]) + log_step, log(W_grid[n_w]) + 5 * log_step, length.out = 5))

        # Calculate only new points and append to the grid
        C_new <- vapply(w_new, cost_function, numeric(1))
        W_grid <- c(W_grid, w_new)
        C_vals <- c(C_vals, C_new)
      } else {
        # Minimum is securely trapped within the grid (Inflection point found!)
        break
      }
    }
    # Extract the final optimum
    optw <- W_grid[which.min(C_vals)]
  }

  # Final density generation
  y_opt <- fftkernel_1d(y_hist, optw / dt)
  y_opt <- y_opt / sum(y_opt * dt)

  # Interpolate to the requested 'tin' grid
  y_final <- stats::approx(t_eval, y_opt, xout = tin, rule = 2)$y

  # Minimalistic output structure
  res <- list(
    x = tin,
    y = y_final,
    optw = optw,
    data = x
  )

  # --- BOOTSTRAP BLOCK (requires boot package) ---
  has_boot <- pkg_has_boot()
  if (nbs > 0 && !has_boot) {
    warning("boot package not available, skipping bootstrap CI")
  }
  if (nbs > 0 && has_boot) {
    # 1. Statistic function for standard nonparametric bootstrap
    kde_boot_stat <- function(data, indices) {
      xb <- data[indices]
      y_histb <- graphics::hist(xb, breaks = breaks, right = FALSE, plot = FALSE)$counts / (N * dt)

      # fftkernel_1d embedded for parallel worker compatibility
      # (not found via package namespace on snow clusters)
      fftkernel_1d <- function(x, w) {
        L <- length(x)
        Lmax <- L + 3 * w
        n <- 2^ceiling(log2(Lmax))
        X <- stats::fft(c(x, numeric(n - L)))
        f <- pmin((0:(n - 1)) / n, 1 - (0:(n - 1)) / n)
        K <- exp(-0.5 * (w * 2 * pi * f)^2)
        y <- Re(stats::fft(X * K, inverse = TRUE)) / n
        y[1:L]
      }

      yb_buf <- fftkernel_1d(y_histb, optw / dt)
      yb_buf <- yb_buf / sum(yb_buf * dt)
      return(stats::approx(t_eval, yb_buf, xout = tin, rule = 2)$y)
    }

    # 2. Multiprocessing setup (clean and cross-platform)
    par_type <- "no"
    if (ncores > 1) {
      par_type <- if (.Platform$OS.type == "windows") "snow" else "multicore"
    }

    # 3. Unified boot call
    boot_out <- boot::boot(
      data = x_valid,
      statistic = kde_boot_stat,
      R = nbs,
      parallel = par_type,
      ncpus = ncores
    )

    # 4. Assemble results
    yb <- boot_out$t
    yb_sort <- apply(yb, 2, sort)
    y95b <- yb_sort[max(1, floor(0.05 * nbs)), ]
    y95u <- yb_sort[floor(0.95 * nbs), ]
    res$confb95 <- rbind(y95b, y95u)
    res$yb <- yb
  }

  class(res) <- "sskernel"
  return(res)
}

#' @title Optimal 2D Kernel Density Estimation (Fixed Bandwidth)
#' @description Computes the optimal global bandwidth for a 2D kernel density estimate
#' based on the exact L2 risk (Mean Integrated Squared Error) minimization.
#'
#' @param x Numeric vector for X coordinates, or a 2-column matrix containing X and Y.
#' @param y Numeric vector for Y coordinates (required if \code{x} is a vector).
#' @param W Optional numeric vector of bandwidths to evaluate. If \code{NULL}, a default log-spaced grid is used.
#' @param n_grid Integer specifying the number of grid points for the output density matrix (default: 100).
#' @param ncores Integer. Number of OpenMP threads to use. Defaults to 1 for CRAN compliance.
#'
#' @return An object of class \code{"sskernel2d"} containing:
#' \describe{
#'   \item{x_grid}{Numeric vector of the X-axis grid points.}
#'   \item{y_grid}{Numeric vector of the Y-axis grid points.}
#'   \item{z}{Numeric matrix of the estimated 2D density.}
#'   \item{opt_wx}{The optimal global bandwidth for the X dimension.}
#'   \item{opt_wy}{The optimal global bandwidth for the Y dimension.}
#'   \item{data}{A list containing the original X and Y data.}
#' }
#' @export
sskernel2d <- function(x, y = NULL, W = NULL, n_grid = 100, ncores = getOption("sshist.ncores", 1L)) {
  if (is.null(y)) { y <- x[, 2]; x <- x[, 1] }
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]; y <- y[valid]

  # Mandatory standardization (Author's recommendation for a 1-parameter optimization window in 2D)
  sd_x <- stats::sd(x); sd_y <- stats::sd(y)
  xn <- (x - mean(x)) / sd_x; yn <- (y - mean(y)) / sd_y

  # 2D Cost function wrapped to C++
  cost_func <- function(w) {
    compute_sskernel2d_cost_cpp(xn, yn, w, ncores)
  }

  # --- BANDWIDTH OPTIMIZATION ---
  if (!is.null(W)) {
    # User-supplied grid: evaluate and pick minimum
    costs <- vapply(W, cost_func, numeric(1))
    opt_w_norm <- W[which.min(costs)]
  } else {
    # Data-driven bounds — analogous to 1D
    # Get distance bounds rapidly via C++
    bounds <- get_tau_bounds_cpp(xn, yn, ncores)

    # 1. Search boundaries. Set a hard limit (preventing calculation at exactly 0)
    Wmin_limit <- sqrt(bounds[1]) / 10
    Wmax_initial <- sqrt(bounds[2])

    # 2. Starting logarithmic grid of 40 points
    W_grid <- exp(seq(log(max(sqrt(bounds[1]) / 4, Wmin_limit)),
                      log(Wmax_initial),
                      length.out = 40))

    C_vals <- vapply(W_grid, cost_func, numeric(1))

    # 3. Smart boundary expansion (Grid Auto-Expansion)
    max_expansions <- 10
    for (expansion in seq_len(max_expansions)) {
      min_idx <- which.min(C_vals)
      n_w <- length(W_grid)

      if (min_idx == 1) {
        # Minimum on the LEFT boundary (moving towards smaller windows)
        if (W_grid[1] <= Wmin_limit * 1.01) break # Protection against physical zero

        log_step <- log(W_grid[2]) - log(W_grid[1])
        w_new <- exp(seq(log(W_grid[1]) - 5 * log_step, log(W_grid[1]) - log_step, length.out = 5))
        w_new <- w_new[w_new >= Wmin_limit]

        if (length(w_new) == 0) break

        # Calculate only new points and prepend to the grid
        C_new <- vapply(w_new, cost_func, numeric(1))
        W_grid <- c(w_new, W_grid)
        C_vals <- c(C_new, C_vals)

      } else if (min_idx == n_w) {
        # Minimum on the RIGHT boundary (moving towards wider windows)
        log_step <- log(W_grid[n_w]) - log(W_grid[n_w - 1])
        w_new <- exp(seq(log(W_grid[n_w]) + log_step, log(W_grid[n_w]) + 5 * log_step, length.out = 5))

        # Calculate only new points and append to the grid
        C_new <- vapply(w_new, cost_func, numeric(1))
        W_grid <- c(W_grid, w_new)
        C_vals <- c(C_vals, C_new)

      } else {
        # Minimum is securely trapped within the grid (Inflection point found!)
        break
      }
    }

    # Extract the final optimum
    opt_w_norm <- W_grid[which.min(C_vals)]
  }

  # Scale optimal bandwidths back to original dimensions
  opt_wx <- opt_w_norm * sd_x
  opt_wy <- opt_w_norm * sd_y

  # Generate 2D density grid
  gx <- seq(min(x), max(x), length.out = n_grid)
  gy <- seq(min(y), max(y), length.out = n_grid)

  # --- C++ GRID COMPUTATION ---
  Z <- compute_kde2d_cpp(x, y, gx, gy, opt_wx, opt_wy, ncores)

  res <- list(
    x_grid = gx,
    y_grid = gy,
    z = Z,
    opt_wx = opt_wx,
    opt_wy = opt_wy,
    data   = list(x = x, y = y)
  )

  class(res) <- "sskernel2d"
  return(res)
}

# --- S3 Methods: sskernel ---

#' Print method for sskernel objects
#'
#' @export
#' @param x An object of class \code{"sskernel"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @return Returns \code{x} invisibly.
print.sskernel <- function(x, ...) {
  cat("Shimazaki-Shinomoto Kernel Density Estimation\n")
  cat("----------------------------------------------\n")
  cat("Optimal Bandwidth:", format(x$optw, digits = 4), "\n")
  cat("Grid Points:      ", length(x$x), "\n")
  if (!is.null(x$confb95))
    cat("Bootstrap CI:      90%  (", nrow(x$yb), "samples)\n")
  invisible(x)
}

#' Plot method for sskernel objects
#'
#' Draws the optimal fixed-bandwidth kernel density curve. When bootstrap
#' confidence intervals are stored in the object (\code{nbs > 0}), a shaded
#' 90% band is added automatically. A rug and jittered points are drawn in a
#' reserved strip BELOW y = 0 (so they never overlap the density curve).
#'
#' @export
#' @param x An object of class \code{"sskernel"}.
#' @param col Color of the density curve (default \code{"#2166ac"}).
#' @param band_col Fill color of the confidence band.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot.default}}.
#' @return No return value; called for its side effect of producing a plot.
plot.sskernel <- function(x,
                          col      = "#2166ac",
                          band_col = adjustcolor(col, alpha.f = 0.2),
                          ...) {
  dots <- list(...)
  if (!"xlab" %in% names(dots)) dots$xlab <- "Data"
  if (!"ylab" %in% names(dots)) dots$ylab <- "Density"
  if (!"main" %in% names(dots)) {
    dots$main <- paste0("Optimal KDE  (bandwidth = ", round(x$optw, 4), ")")
  }

  dots$x <- x$x
  dots$y <- x$y
  dots$type <- "l"
  dots$lwd <- 2
  dots$col <- col

  # Reserve a bottom strip (~10% of y_max below zero) for the data strip
  # (rug + jittered points), consistent with plot.sshist / plot.ssvkernel.
  if (!"ylim" %in% names(dots)) {
    max_y <- max(x$y, if (!is.null(x$confb95)) x$confb95[2L, ] else NULL, na.rm = TRUE)
    dots$ylim <- c(-max_y * 0.10, max_y * 1.05)
  }

  # Suppress the default y-axis: the negative part of ylim is reserved for
  # the data strip and should NOT show negative density ticks.
  dots$yaxt <- "n"

  # Main plot
  do.call(plot, dots)

  # Redraw a clean y-axis with only non-negative ticks
  usr <- par("usr")
  y_ticks <- pretty(c(0, usr[4]))
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= usr[4]]
  axis(2, at = y_ticks, las = 1)

  # Subtle separator line at the x-axis (y = 0)
  abline(h = 0, col = "gray70", lwd = 0.5)

  # Confidence interval band
  if (!is.null(x$confb95)) {
    polygon(c(x$x, rev(x$x)),
            c(x$confb95[1L, ], rev(x$confb95[2L, ])),
            col = band_col, border = NA)
    # Redraw the density line over the polygon so it doesn't get muted
    lines(x$x, x$y, lwd = 2, col = col)
  }

  # --- DATA PLOTTING BLOCK ---
  # 1. Rug at the bottom of the reserved strip
  rug(x$data, col = adjustcolor("gray40", alpha.f = 0.5))

  # 2. Jittered points ABOVE the rug, but BELOW y = 0.
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
    col = adjustcolor(col, alpha.f = 0.4)
  )
}

# --- S3 Methods: sskernel2d ---

#' Print method for sskernel2d objects
#'
#' @export
#' @param x An object of class \code{"sskernel2d"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @return Returns \code{x} invisibly.
print.sskernel2d <- function(x, ...) {
  cat("Shimazaki-Shinomoto 2D Kernel Density Estimation\n")
  cat("-------------------------------------------------\n")
  cat("Optimal Bandwidth X:", format(x$opt_wx, digits = 4), "\n")
  cat("Optimal Bandwidth Y:", format(x$opt_wy, digits = 4), "\n")
  cat("Grid Size:          ", length(x$x_grid), "x", length(x$y_grid), "\n")
  invisible(x)
}

#' Plot method for sskernel2d objects
#'
#' Draws the 2D kernel density estimate as a heatmap with optional data overlay.
#'
#' @export
#' @param x An object of class \code{"sskernel2d"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{image}}.
#' @return No return value; called for its side effect of producing a plot.
plot.sskernel2d <- function(x, ...) {
  dots <- list(...)
  if (!"xlab" %in% names(dots)) dots$xlab <- "X"
  if (!"ylab" %in% names(dots)) dots$ylab <- "Y"
  if (!"main" %in% names(dots)) {
    dots$main <- sprintf(
      "2D KDE - Fixed Bandwidth  (wx = %.3f,  wy = %.3f)",
      x$opt_wx, x$opt_wy)
  }
  dots$x <- x$x_grid
  dots$y <- x$y_grid
  dots$z <- x$z
  dots$col <- hcl.colors(128, "Inferno")

  do.call(image, dots)
  points(x$data$x, x$data$y, pch = 20, col = rgb(1, 1, 1, 0.35), cex = 0.7)
  box()
}
