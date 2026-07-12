options(sshist.ncores = 2)

# ── Reference value tests ─────────────────────────────────────────────────────

test_that("sskernel(waiting) matches Python reference", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel(df$waiting)
  dt  <- min(diff(res$x))
  expect_equal(res$optw, 2.85, tolerance = 1e-3)
  expect_equal(length(res$x), 53L)
  expect_equal(res$x[1],  43.0, tolerance = 1e-6)
  expect_equal(res$x[53], 96.0, tolerance = 1e-6)
  expect_equal(res$y[1],  0.00470, tolerance = 1e-4)
  expect_equal(res$y[53], 0.0022434, tolerance = 1e-4)
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-3)
})

test_that("sskernel(eruptions) matches Python reference", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel(df$eruptions)
  dt  <- min(diff(res$x))
  expect_equal(res$optw, 0.1103, tolerance = 1e-3)
  expect_equal(length(res$x), 1000L)
  expect_equal(res$x[1],  1.60, tolerance = 1e-6)
  expect_equal(res$x[1000L], 5.10, tolerance = 1e-6)
  expect_equal(res$y[1], 0.116, tolerance = 1e-4)
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-3)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("sskernel errors on non-numeric or insufficient data", {
  expect_error(suppressWarnings(sskernel(letters)))
  expect_error(suppressWarnings(sskernel(numeric(0))))
  expect_error(sskernel(1))
})

test_that("sskernel removes NAs silently", {
  res <- sskernel(c(1:20, NA, NA, 21:50))
  expect_s3_class(res, "sskernel")
  expect_equal(length(res$data), 50L)
})

# ── Parameter variations ──────────────────────────────────────────────────────

test_that("sskernel accepts custom tin grid", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  tin <- seq(43, 96, length.out = 100)
  res <- sskernel(df$waiting, tin = tin)
  expect_equal(length(res$x), 100L)
  expect_equal(res$x, tin)
})

test_that("sskernel accepts user-supplied bandwidth grid W", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  W   <- seq(0.5, 10, length.out = 20)
  res <- sskernel(df$waiting, W = W)
  expect_s3_class(res, "sskernel")
  expect_true(res$optw >= min(W) && res$optw <= max(W))
})

test_that("sskernel bootstrap produces correct structure", {
  skip_if_not_installed("boot")
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  set.seed(42)
  res <- sskernel(df$waiting, nbs = 20)
  expect_true(!is.null(res$confb95))
  expect_true(!is.null(res$yb))
  expect_equal(nrow(res$yb), 20L)
  expect_equal(ncol(res$yb), length(res$x))
  expect_true(all(res$confb95[2, ] >= res$confb95[1, ]))
})

# ── Return value structure ────────────────────────────────────────────────────

test_that("sskernel returns correct structure", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel(df$waiting)
  expect_named(res, c("x", "y", "optw", "data"))
  expect_type(res$optw, "double")
  expect_true(res$optw > 0)
})

# ── Bootstrap warning when boot is unavailable ──────────────────────────────

test_that("sskernel warns when boot is not available", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  local_mocked_bindings(pkg_has_boot = function() FALSE)
  expect_warning(sskernel(df$waiting, nbs = 20), "boot package not available")
})

# ── S3 methods ────────────────────────────────────────────────────────────────

test_that("print.sskernel runs without error", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel(df$waiting)
  expect_output(print(res), "Optimal Bandwidth")
  expect_invisible(print(res))
})

test_that("plot.sskernel runs without error", {
  pdf(NULL)
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel(df$waiting)
  expect_silent(plot(res))
  dev.off()
})

test_that("plot.sskernel works with bootstrap confidence band", {
  skip_if_not_installed("boot")
  pdf(NULL)
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  set.seed(42)
  res <- sskernel(df$waiting, nbs = 10)
  expect_silent(plot(res))
  dev.off()
})

# ── Iris dataset ─────────────────────────────────────────────────────────────

test_that("sskernel returns expected values for iris columns", {
  res <- sskernel(iris$Sepal.Length)
  expect_equal(res$optw, 0.38968, tolerance = 1e-4)
  expect_equal(length(res$x), 37L)
  expect_equal(res$x[1], 4.3, tolerance = 1e-6)
  expect_equal(res$x[37], 7.9, tolerance = 1e-6)
  expect_equal(res$y[1], 0.117690, tolerance = 1e-5)
  dt <- min(diff(res$x))
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-6)

  res <- sskernel(iris$Sepal.Width)
  expect_equal(res$optw, 0.2, tolerance = 1e-6)
  expect_equal(length(res$x), 25L)
  expect_equal(res$y[1], 0.066571, tolerance = 1e-5)
  dt <- min(diff(res$x))
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-6)

  res <- sskernel(iris$Petal.Length)
  expect_equal(res$optw, 0.2, tolerance = 1e-6)
  expect_equal(length(res$x), 60L)

  res <- sskernel(iris$Petal.Width)
  expect_equal(res$optw, 0.2, tolerance = 1e-6)
  expect_equal(length(res$x), 25L)
  expect_equal(res$y[1], 0.540645, tolerance = 1e-5)
  dt <- min(diff(res$x))
  expect_equal(sum(res$y) * dt, 1.0, tolerance = 1e-6)
})
