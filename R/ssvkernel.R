#' @title Locally Adaptive 1D Kernel Density Estimation (Shimazaki-Shinomoto)
#' @description Computes locally adaptive bandwidths for 1D distributions
#' following the method of Shimazaki & Shinomoto (2010). Optimizes the
#' stiffness constant gamma via a hybrid grid and Brent search on the MISE cost.
#'
#' @param x Numeric vector of sample points. Missing values (NA) will be removed.
#' @param tin Optional numeric vector of evaluation points.
#' @param M Integer, number of bandwidths to examine (default: 80).
#' @param WinFunc Character string specifying the window function for local weights: "Gauss", "Boxcar", "Laplace", or "Cauchy" (default: "Boxcar").
#' @param nbs Integer, number of bootstrap samples for confidence intervals. Set to 0 to skip (default: 0).
#' @param ncores Integer specifying the number of CPU cores to use for bootstrap (default: 1).
#'
#' @return An object of class \code{"ssvkernel"} containing:
#' \describe{
#'   \item{x}{The evaluation points (same as \code{tin}).}
#'   \item{y}{The optimized adaptive density estimate.}
#'   \item{optw_local_min}{Numeric vector of locally adaptive bandwidths evaluated at \code{x}.}
#'   \item{optw_local_max}{Numeric vector of locally adaptive bandwidths evaluated at \code{x}.}
#'   \item{gamma}{The optimal stiffness constant.}
#'   \item{data}{The original evaluated data.}
#'   \item{confb95}{(If nbs > 0) A matrix with 5th and 95th percentile bootstrap confidence intervals.}
#'   \item{yb}{(If nbs > 0) A matrix of all bootstrap density samples.}
#' }
#' @export
ssvkernel <- function(x, tin = NULL, M = 80, WinFunc = "Boxcar", nbs = 0, ncores = getOption("sshist.ncores", 1L)) {
  x <- sort(stats::na.omit(x))

  # 1. GRID PROTECTION: Ensure at least 256 points for estimation smoothness,
  # even if the data is highly discrete (e.g., integer minutes)
  if (is.null(tin)) {
    raw_points <- ceiling((max(x) - min(x)) / min(diff(unique(x))))
    n_points <- max(256, min(raw_points, 1000))
    tin <- seq(min(x), max(x), length.out = n_points)
  } else if (length(tin) < 2) {
    stop("Argument 'tin' must have at least 2 points for evaluation.")
  }

  x_ab <- x[x >= min(tin) & x <= max(tin)]
  if (length(x_ab) < 2) stop("Need at least 2 data points in the range of tin.")
  N <- length(x_ab)

  dx <- diff(sort(x_ab))
  dx <- dx[dx > 0]
  dt_samp <- if (length(dx) > 0) min(dx) else (max(x_ab) - min(x_ab)) / 1000

  if (dt_samp > min(diff(tin))) {
    n_t <- max(256, min(ceiling((max(tin) - min(tin)) / dt_samp), 1000))
    t <- seq(min(tin), max(tin), length.out = n_t)
  } else {
    t <- tin
  }
  dt <- min(diff(t))

  breaks <- c(t - dt/2, t[length(t)] + dt/2)
  # 'right = FALSE' strictly mimics Python's np.histogram half-open bins [a, b)
  y_hist <- graphics::hist(x_ab, breaks = breaks, right = FALSE, plot = FALSE)$counts / dt
  L <- length(y_hist)

  # --- NONLINEAR (LOG-EXP) WINDOW GENERATION FROM PYTHON ---
  logexp <- function(z) ifelse(z < 1e2, log(1 + exp(z)), z)
  ilogexp <- function(z) ifelse(z < 1e2, log(exp(z) - 1), z)

  T_span <- max(t) - min(t)

  # 2. SEARCH BOUNDARY PROTECTION (Statistical Floor)
  sd_x <- stats::sd(x_ab)
  iqr_x <- stats::IQR(x_ab)
  scale_x <- if (iqr_x > 0) min(sd_x, iqr_x / 1.34) else sd_x
  if (scale_x == 0) scale_x <- 1
  w_rot <- 1.06 * scale_x * N^(-0.2)

  # The bandwidth must never be smaller than the physical
  # sampling resolution of the data (dt_samp) to prevent the Dirac comb effect.
  Wmin_limit <- max(2 * dt, w_rot / 10, dt_samp)

  # Build a safe candidate matrix W
  W_vals <- logexp(seq(ilogexp(Wmin_limit), ilogexp(T_span), length.out = M))
  WIN <- W_vals

  # Matrix of local costs c (M x L) -- Equation (15)
  c_matrix <- matrix(0, M, L)
  for (j in 1:M) {
    w <- W_vals[j]
    yh <- fftkernel_1d(y_hist, w / dt)
    c_matrix[j, ] <- yh^2 - 2 * yh * y_hist + (2 / (sqrt(2 * pi) * w)) * y_hist
  }

  # Find locally optimal fixed bandwidths optws (M x L)
  optws <- matrix(0, M, L)
  for (i in 1:M) {
    Win <- WIN[i]
    C_local <- t(apply(c_matrix, 1, function(row) fftkernel_win(row, Win / dt, WinFunc)))
    optws[i, ] <- W_vals[apply(C_local, 2, which.min)]
  }

  # --- Global Search (Grid Search) ---
  n_grid <- 15
  gamma_grid <- seq(1e-4, 1, length.out = n_grid)

  C_coarse <- vapply(gamma_grid, function(g) {
    CostFunction(y_hist, N, t, dt, optws, WIN, WinFunc, g)$Cg
  }, numeric(1))

  best_idx <- which.min(C_coarse)
  lower_b <- if (best_idx == 1) 1e-12 else gamma_grid[best_idx - 1]
  upper_b <- if (best_idx == n_grid) 1 else gamma_grid[best_idx + 1]

  # --- Precise Refinement (Brent's Method) ---
  opt_res <- stats::optimize(
    f = function(g) CostFunction(y_hist, N, t, dt, optws, WIN, WinFunc, g)$Cg,
    interval = c(lower_b, upper_b),
    tol = 1e-5
  )

  best_gamma <- opt_res$minimum

  # --- Final Calculation ---
  f_final <- CostFunction(y_hist, N, t, dt, optws, WIN, WinFunc, best_gamma)
  yopt <- f_final$yv / sum(f_final$yv * dt)
  optw <- f_final$optwp

  gs_history <- c(gamma_grid, best_gamma)
  C_history  <- c(C_coarse, opt_res$objective)

  # Interpolate to the requested 'tin' grid
  y_final <- stats::approx(t, yopt, xout = tin, rule = 2)$y
  optw_final <- stats::approx(t, optw, xout = tin, rule = 2)$y

  result <- list(
    x              = tin,
    y              = y_final,
    optw_local_min = as.numeric(min(optw_final, na.rm = TRUE)),
    optw_local_max = as.numeric(max(optw_final, na.rm = TRUE)),
    gamma          = best_gamma,
    data           = x
  )

  has_boot <- pkg_has_boot()
  if (nbs > 0 && !has_boot) {
    warning("boot package not available, skipping bootstrap CI")
  }
  if (nbs > 0 && has_boot) {
    # 1. Sample generator for parametric Poisson bootstrap
    ran_gen_poisson <- function(data, mle) {
      Ni <- stats::rpois(1, length(data))
      data[sample.int(length(data), size = Ni, replace = TRUE)]
    }

    # 2. Statistic function
    kde_boot_stat_v <- function(data) {
      y_histb <- graphics::hist(data, breaks = breaks, right = FALSE, plot = FALSE)$counts / dt
      idx_nz <- which(y_histb > 0)

      # Protection against empty bins
      if (length(idx_nz) == 0) return(rep(0, length(tin)))

      y_histb_nz <- y_histb[idx_nz]
      t_nz <- t[idx_nz]
      yb_buf <- numeric(L)

      # The Balloon Density Estimator from the original algorithm
      # The density kernel is always Gaussian per the original algorithm
      for (k_idx in 1:L) {
        yb_buf[k_idx] <- sum(y_histb_nz * dt * stats::dnorm(t[k_idx] - t_nz, sd = optw[k_idx]))
      }
      yb_buf <- yb_buf / sum(yb_buf * dt)

      return(stats::approx(t, yb_buf, xout = tin, rule = 2)$y)
    }

    # 3. Multiprocessing setup
    par_type <- "no"
    if (ncores > 1) {
      par_type <- if (.Platform$OS.type == "windows") "snow" else "multicore"
    }

    # 4. Unified boot call
    boot_out <- boot::boot(
      data = x_ab,
      statistic = kde_boot_stat_v,
      R = nbs,
      sim = "parametric",
      ran.gen = ran_gen_poisson,
      mle = NULL, # The 'mle' parameter is required by the boot API but is not used here
      parallel = par_type,
      ncpus = ncores
    )

    # 5. Assemble results
    yb <- boot_out$t
    yb_sort <- apply(yb, 2, sort)
    confb95 <- rbind(yb_sort[max(1, floor(0.05 * nbs)), ], yb_sort[floor(0.95 * nbs), ])

    result$confb95 <- confb95
    result$yb <- yb
  }

  class(result) <- "ssvkernel"
  return(result)
}

# ==============================================================================
# --- Internal Helper Functions ---
# ==============================================================================

#' Internal helper: Compute the cost function for a given gamma
#' @keywords internal
#' @noRd
CostFunction <- function(y_hist, N, t, dt, optws, WIN, WinFunc, g) {
  L <- length(y_hist)
  optwv <- numeric(L)
  for (k in 1:L) {
    gs <- optws[, k] / WIN
    if (g > max(gs)) {
      optwv[k] <- min(WIN)
    } else if (g < min(gs)) {
      optwv[k] <- max(WIN)
    } else {
      idx <- max(which(gs >= g))
      optwv[k] <- g * WIN[idx]
    }
  }

  # Nadaraya-Watson smoothing with the selected weight function
  optwp <- numeric(L)
  for (k in 1:L) {
    Z <- weight_fun(t[k] - t, optwv / g, WinFunc)
    optwp[k] <- sum(optwv * Z) / sum(Z)
  }

  # Balloon density estimator (kernel is always Gaussian)
  yv <- numeric(L)
  idx_nz <- which(y_hist > 0)
  y_hist_nz <- y_hist[idx_nz]
  t_nz <- t[idx_nz]

  for (k in 1:L) {
    yv[k] <- sum(y_hist_nz * dt * stats::dnorm(t[k] - t_nz, mean = 0, sd = optwp[k]))
  }

  yv <- yv * N / sum(yv * dt)

  cg <- yv^2 - 2 * yv * y_hist + (2 / (sqrt(2 * pi) * optwp)) * y_hist
  Cg <- sum(cg * dt)

  list(Cg = Cg, yv = yv, optwp = optwp)
}

#' Internal helper: FFT convolution with a specified kernel window
#' @keywords internal
#' @noRd
fftkernel_win <- function(x, w, WinFunc = "Boxcar") {
  L <- length(x)
  Lmax <- L + 3 * w
  n <- 2^ceiling(log2(Lmax))
  X <- stats::fft(c(x, numeric(n - L)))

  # Symmetric frequency vector aligned with R's fft() structure
  f <- pmin((0:(n - 1)) / n, 1 - (0:(n - 1)) / n)

  # Frequency response of the selected window
  if (WinFunc == "Boxcar") {
    a <- sqrt(12) * w
    t <- 2 * pi * f
    K <- 2 * sin(a * t/2) / (a * t)
    K[1] <- 1   # Prevent NaN at zero frequency
  } else if (WinFunc == "Laplace") {
    K <- 1 / (1 + (w * 2 * pi * f)^2 / 2)
  } else if (WinFunc == "Cauchy") {
    K <- exp(-w * abs(2 * pi * f))
  } else { # Gauss
    K <- exp(-0.5 * (w * 2 * pi * f)^2)
  }

  y <- Re(stats::fft(X * K, inverse = TRUE)) / n
  return(y[1:L])
}

#' Internal helper: Weight function values for Nadaraya-Watson regression
#' @keywords internal
#' @noRd
weight_fun <- function(x, w, WinFunc = "Boxcar") {
  if (WinFunc == "Boxcar") {
    a <- sqrt(12) * w
    return( (abs(x) <= a/2) / a )
  } else if (WinFunc == "Laplace") {
    return( 1/(sqrt(2)*w) * exp(-sqrt(2)/w * abs(x)) )
  } else if (WinFunc == "Cauchy") {
    return( 1 / (pi * w * (1 + (x/w)^2)) )
  } else {  # Gauss
    return( stats::dnorm(x, mean = 0, sd = w) )
  }
}

#' @title Locally Adaptive 2D Kernel Density Estimation (Abramson's Method)
#' @description Computes a 2D kernel density estimate using Abramson's square-root
#' adaptive bandwidths. The procedure is initialized by finding the optimal global
#' bandwidth via \code{\link{sskernel2d}}.
#'
#' @param x Numeric vector for X coordinates, or a 2-column matrix containing X and Y.
#' @param y Numeric vector for Y coordinates (required if \code{x} is a vector).
#' @param n_grid Integer specifying the number of grid points for the output density matrix (default: 100).
#' @param sensitivity Numeric scalar controlling the sensitivity of local bandwidths
#' to the pilot density. A value of 0.5 (default) corresponds to Abramson's original inverse square-root law.
#' @param ncores Integer. Number of OpenMP threads to use. Defaults to 1 for CRAN compliance.
#'
#' @return An object of class \code{"ssvkernel2d"} containing:
#' \describe{
#'   \item{x_grid}{Numeric vector of the X-axis grid points.}
#'   \item{y_grid}{Numeric vector of the Y-axis grid points.}
#'   \item{z}{Numeric matrix of the estimated adaptive 2D density.}
#'   \item{pilot_wx}{The global optimal pilot bandwidth for the X dimension.}
#'   \item{pilot_wy}{The global optimal pilot bandwidth for the Y dimension.}
#'   \item{lambda_factors}{Numeric vector of local scaling factors applied to each data point.}
#'   \item{data}{A list containing the original X and Y data.}
#' }
#' @export
ssvkernel2d <- function(x, y = NULL, n_grid = 100, sensitivity = 0.5, ncores = getOption("sshist.ncores", 1L)) {
  if (is.null(y)) { y <- x[, 2]; x <- x[, 1] }
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]; y <- y[valid]
  N <- length(x)

  # 1. Obtain the pilot estimate (Global Optimal KDE)
  pilot <- sskernel2d(x, y, n_grid = n_grid)

  # 2. Evaluate the pilot density exactly at the data points (via C++)
  pilot_density_at_points <- compute_pilot_density_cpp(x, y, pilot$opt_wx, pilot$opt_wy, ncores)

  # Adaptive scaling coefficients (Abramson's Law)
  # Lambda is inversely proportional to the square root of the density (controlled by sensitivity).
  # The geometric mean g is computed only over points with positive pilot density;
  # using full N when some values are zero would underestimate g and inflate lambda.
  pos <- pilot_density_at_points > 0
  g   <- exp(sum(log(pilot_density_at_points[pos])) / sum(pos))
  lambda <- (pilot_density_at_points / g) ^ (-sensitivity)

  # Local bandwidths for each specific point
  wx_local <- pilot$opt_wx * lambda
  wy_local <- pilot$opt_wy * lambda

  # 3. Compute the final adaptive density on the grid (via C++)
  Z_adaptive <- compute_kde2d_cpp(x, y, pilot$x_grid, pilot$y_grid, wx_local, wy_local, ncores)

  res <- list(
    x_grid         = pilot$x_grid,
    y_grid         = pilot$y_grid,
    z              = Z_adaptive,
    pilot_wx       = pilot$opt_wx,
    pilot_wy       = pilot$opt_wy,
    lambda_factors = lambda,
    data           = list(x = x, y = y)
  )

  class(res) <- "ssvkernel2d"
  return(res)
}

# --- S3 Methods: ssvkernel ---

#' Print method for ssvkernel objects
#'
#' @export
#' @param x An object of class \code{"ssvkernel"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @return Returns \code{x} invisibly.
print.ssvkernel <- function(x, ...) {
  cat("Shimazaki-Shinomoto Adaptive Kernel Density Estimation\n")
  cat("-------------------------------------------------------\n")
  cat("Optimal Stiffness (gamma):", format(x$gamma, digits = 4), "\n")
  cat("Grid Points:              ", length(x$x), "\n")
  cat("Bandwidth Range:          ",
      x$optw_local_min, "--",
      x$optw_local_max, "\n")
  if (!is.null(x$confb95))
    cat("Bootstrap CI:              90%  (", nrow(x$yb), "samples)\n")
  invisible(x)
}

#' Plot method for ssvkernel objects
#'
#' Draws the locally adaptive kernel density curve. When bootstrap confidence
#' intervals are stored (\code{nbs > 0}), a shaded 90% band is added.
#' Raw data points and a rug are plotted along the x-axis.
#'
#' @export
#' @param x An object of class \code{"ssvkernel"}.
#' @param col Color of the density curve (default \code{"#d6604d"}).
#' @param bw_col Color of the local bandwidth line (default \code{"#4d9221"}).
#' @param band_col Fill color of the confidence band.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot.default}}.
#' @return No return value; called for its side effect of producing a plot.
plot.ssvkernel <- function(x,
                           col      = "#d6604d",
                           bw_col   = "#4d9221",
                           band_col = adjustcolor(col, alpha.f = 0.2),
                           ...) {
  has_boot <- !is.null(x$confb95)

  dots <- list(...)
  if (!"xlab" %in% names(dots)) dots$xlab <- "Data"
  if (!"ylab" %in% names(dots)) dots$ylab <- "Density"
  if (!"main" %in% names(dots)) {
    dots$main <- paste0("Adaptive KDE  (gamma = ", round(x$gamma, 4), ")")
  }

  xlab_val <- dots$xlab
  ylab_val <- dots$ylab

  dots_top <- dots
  dots_top$x <- x$x
  dots_top$y <- x$y
  dots_top$type <- "l"
  dots_top$lwd <- 2
  dots_top$col <- col

  # Ensure the Y-axis starts at 0
  if (!"ylim" %in% names(dots_top)) {
    max_y <- max(x$y, if (has_boot) x$confb95[2L, ] else NULL, na.rm = TRUE)
    dots_top$ylim <- c(0, max_y * 1.05)
  }

  # Main plot
  do.call(plot, dots_top)

  # 1. Rug plot
  rug(x$data, col = adjustcolor("gray40", alpha.f = 0.5))

  # 2. Jittered points
  # Get current plot area coordinates
  usr <- par("usr")
  y_range <- usr[4] - usr[3]

  # Elevate the base line for points to 2% of the plot height
  y_base <- rep(usr[3] + y_range * 0.02, length(x$data))

  points(
    x = x$data,
    # Jitter scatters points up/down by 1.5% of the plot height
    y = jitter(y_base, amount = y_range * 0.015),
    pch = 16,
    cex = 0.6,
    col = adjustcolor(col, alpha.f = 0.4)
  )

  # Confidence interval band
  if (has_boot) {
    polygon(c(x$x, rev(x$x)),
            c(x$confb95[1L, ], rev(x$confb95[2L, ])),
            col = band_col, border = NA)
    # Redraw the density line over the polygon
    lines(x$x, x$y, lwd = 2, col = col)
  }
}

# --- S3 Methods: ssvkernel2d ---

#' Print method for ssvkernel2d objects
#'
#' @export
#' @param x An object of class \code{"ssvkernel2d"}.
#' @param ... Additional arguments passed to \code{\link[base]{print}}.
#' @return Returns \code{x} invisibly.
print.ssvkernel2d <- function(x, ...) {
  lam <- x$lambda_factors
  cat("Shimazaki-Shinomoto Adaptive 2D Kernel Density Estimation\n")
  cat("----------------------------------------------------------\n")
  cat("Pilot Bandwidth X:  ", format(x$pilot_wx, digits = 4), "\n")
  cat("Pilot Bandwidth Y:  ", format(x$pilot_wy, digits = 4), "\n")
  cat("Grid Size:          ", length(x$x_grid), "x", length(x$y_grid), "\n")
  cat("Lambda Range:       ",
      format(min(lam), digits = 3), "--", format(max(lam), digits = 3),
      "  (median", format(median(lam), digits = 3), ")\n")
  invisible(x)
}

#' Plot method for ssvkernel2d objects
#'
#' Draws the locally adaptive 2D kernel density as a heatmap with optional
#' data point overlay.
#'
#' @export
#' @param x An object of class \code{"ssvkernel2d"}.
#' @param ... Additional arguments passed to \code{\link[graphics]{image}}.
#' @return No return value; called for its side effect of producing a plot.
plot.ssvkernel2d <- function(x, ...) {
  dots <- list(...)
  if (!"xlab" %in% names(dots)) dots$xlab <- "X"
  if (!"ylab" %in% names(dots)) dots$ylab <- "Y"
  if (!"main" %in% names(dots)) {
    dots$main <- sprintf(
      "2D KDE - Adaptive Bandwidth  (pilot wx = %.3f,  wy = %.3f)",
      x$pilot_wx, x$pilot_wy)
  }
  dots$x <- x$x_grid
  dots$y <- x$y_grid
  dots$z <- x$z
  dots$col <- hcl.colors(128, "Inferno")

  do.call(image, dots)
  points(x$data$x, x$data$y, pch = 20, col = rgb(1, 1, 1, 0.35), cex = 0.7)
  box()
}
