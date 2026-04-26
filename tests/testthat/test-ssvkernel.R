options(sshist.ncores = 2)

# ── Reference value tests ─────────────────────────────────────────────────────

test_that("ssvkernel(waiting) matches Python reference", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$waiting)
  dt  <- min(diff(res$x))
  expect_equal(length(res$x), 256L)
  expect_equal(res$x[1],   43.0, tolerance = 1e-1)
  expect_equal(res$x[256], 96.0, tolerance = 1e-1)
  expect_equal(res$y[1],   0.0053319, tolerance = 1e-3)
  expect_equal(res$y[256], 0.0026692, tolerance = 1e-3)
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-4)
  expect_equal(as.numeric(res$optw_local_min), 1.2511, tolerance = 1e-2)
  expect_equal(as.numeric(res$optw_local_max), 3.5700, tolerance = 1e-2)
  expect_equal(res$gamma, 0.7290, tolerance = 1e-3)
})

test_that("ssvkernel(eruptions) matches Python reference", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$eruptions)
  dt  <- min(diff(res$x))
  expect_equal(length(res$x), 1000L)
  expect_equal(res$x[1],    1.60, tolerance = 1e-3)
  expect_equal(res$x[1000], 5.10, tolerance = 1e-3)
  expect_equal(res$y[1],    0.06628, tolerance = 1e-3)
  expect_equal(res$y[1000], 0.11289, tolerance = 1e-3)
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-4)
  expect_equal(res$gamma, 0.99999, tolerance = 1e-4)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("ssvkernel errors on insufficient data", {
  expect_error(suppressWarnings(ssvkernel(numeric(0))))
  expect_error(suppressWarnings(ssvkernel(1)))
  expect_error(suppressWarnings(ssvkernel(letters)))
})

test_that("ssvkernel removes NAs silently", {
  res <- ssvkernel(c(1:30, NA, NA, 31:60))
  expect_s3_class(res, "ssvkernel")
  expect_equal(length(res$data), 60L)
})

test_that("ssvkernel errors with tin < 2 points", {
  expect_error(ssvkernel(1:10, tin = 5))
})

# ── Parameter variations ──────────────────────────────────────────────────────

test_that("ssvkernel accepts custom tin grid", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  tin <- seq(43, 96, length.out = 150)
  res <- ssvkernel(df$waiting, tin = tin)
  expect_equal(length(res$x), 150L)
  expect_equal(res$x, tin)
})

test_that("ssvkernel works with all window functions", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  for (wf in c("Gauss", "Boxcar", "Laplace", "Cauchy")) {
    res <- ssvkernel(df$waiting, WinFunc = wf)
    expect_s3_class(res, "ssvkernel")
    expect_true(res$gamma > 0)
  }
})

test_that("ssvkernel works with different M values", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$waiting, M = 30)
  expect_s3_class(res, "ssvkernel")
  expect_true(res$gamma > 0)
})

test_that("ssvkernel bootstrap produces correct structure", {
  skip_if_not_installed("boot")
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  set.seed(42)
  res <- ssvkernel(df$waiting, nbs = 15)
  expect_true(!is.null(res$confb95))
  expect_true(!is.null(res$yb))
  expect_equal(nrow(res$yb), 15L)
  expect_equal(ncol(res$yb), length(res$x))
  expect_true(all(res$confb95[2, ] >= res$confb95[1, ]))
})

# ── Mathematical properties ───────────────────────────────────────────────────

test_that("ssvkernel densities are non-negative and integrate to 1", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$waiting)
  dt  <- min(diff(res$x))
  expect_true(all(res$y >= 0))
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-4)
  expect_true(res$optw_local_min > 0)
  expect_true(res$optw_local_max > 0)
})

# ── Return value structure ────────────────────────────────────────────────────

test_that("ssvkernel returns correct structure", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$waiting)
  expect_named(res, c("x", "y", "optw_local_min", "optw_local_max", "gamma", "data"))
  expect_type(res$gamma, "double")
})

# ── Bootstrap warning when boot is unavailable ──────────────────────────────

test_that("ssvkernel warns when boot is not available", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  local_mocked_bindings(pkg_has_boot = function() FALSE)
  expect_warning(ssvkernel(df$waiting, nbs = 15), "boot package not available")
})

# ── S3 methods ────────────────────────────────────────────────────────────────

test_that("print.ssvkernel runs without error", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$waiting)
  expect_output(print(res), "Optimal Stiffness")
  expect_invisible(print(res))
})

test_that("plot.ssvkernel runs without error", {
  pdf(NULL)
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel(df$waiting)
  expect_silent(plot(res))
  dev.off()
})

test_that("plot.ssvkernel works with bootstrap confidence band", {
  skip_if_not_installed("boot")
  pdf(NULL)
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  set.seed(42)
  res <- ssvkernel(df$waiting, nbs = 10)
  expect_silent(plot(res))
  dev.off()
})
