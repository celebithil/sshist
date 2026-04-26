options(sshist.ncores = 2)

# ── Input validation ──────────────────────────────────────────────────────────

test_that("ssvkernel2d errors on insufficient data", {
  expect_error(ssvkernel2d(1, 2))
})

test_that("ssvkernel2d errors on non-numeric input", {
  expect_error(suppressWarnings(ssvkernel2d(letters, 1:10)))
})

test_that("ssvkernel2d removes NAs silently", {
  x <- c(1:20, NA, NA); y <- c(21:40, NA, NA)
  res <- ssvkernel2d(x, y, n_grid = 10)
  expect_s3_class(res, "ssvkernel2d")
})

# ── Input flexibility ─────────────────────────────────────────────────────────

test_that("ssvkernel2d accepts two vectors and matrix input", {
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  rv <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 30)
  rm <- ssvkernel2d(cbind(df$eruptions, df$waiting), n_grid = 30)
  expect_s3_class(rv, "ssvkernel2d")
  expect_s3_class(rm, "ssvkernel2d")
})

# ── Structure and field checks ────────────────────────────────────────────────

test_that("ssvkernel2d returns valid S3 class and expected fields", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 50L)
  expect_s3_class(res, "ssvkernel2d")
  expect_true(all(c("x_grid", "y_grid", "z", "pilot_wx", "pilot_wy", "lambda_factors") %in% names(res)))
})

test_that("ssvkernel2d output dimensions match n_grid", {
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  for (ng in c(20L, 50L)) {
    res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = ng)
    expect_length(res$x_grid, ng)
    expect_length(res$y_grid, ng)
    expect_equal(dim(res$z), c(ng, ng))
  }
})

# ── Abramson scaling properties ───────────────────────────────────────────────

test_that("Abramson lambda factors match sample size and are strictly positive", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 50L, sensitivity = 0.5)
  expect_length(res$lambda_factors, nrow(df))
  expect_true(all(res$lambda_factors > 0))
  expect_true(all(is.finite(res$lambda_factors)))
})

test_that("Changing sensitivity alters lambda distribution", {
  df     <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res_sq <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 40L, sensitivity = 0.5)
  res_ln <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 40L, sensitivity = 1.0)
  expect_false(isTRUE(all.equal(res_sq$lambda_factors, res_ln$lambda_factors)))
})

# ── Mathematical properties ───────────────────────────────────────────────────

test_that("ssvkernel2d density is non-negative and integrates approx to 1", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 80L)
  expect_true(all(res$z >= 0))
  dx <- min(diff(res$x_grid)); dy <- min(diff(res$y_grid))
  expect_equal(sum(res$z) * dx * dy, 1.0, tolerance = 5e-2)
})

# ── Reference values ──────────────────────────────────────────────────────────

test_that("ssvkernel2d reproduces reference regression values", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 100L, sensitivity = 0.5)
  expect_equal(res$pilot_wx, 0.1947, tolerance = 1e-3)
  expect_equal(res$pilot_wy, 2.319,  tolerance = 1e-3)
  expect_equal(max(res$z),   0.0525, tolerance = 1e-3)
  expect_equal(min(res$z),   0,      tolerance = 1e-3)
  expect_equal(min(res$x_grid), 1.6, tolerance = 1e-3)
  expect_equal(max(res$x_grid), 5.1, tolerance = 1e-3)
  expect_equal(min(res$y_grid), 43.0, tolerance = 1e-3)
  expect_equal(max(res$y_grid), 96.0, tolerance = 1e-3)
  expect_equal(mean(res$lambda_factors), 1.070, tolerance = 1e-2)
  expect_equal(min(res$lambda_factors), 0.653, tolerance = 1e-3)
  expect_equal(max(res$lambda_factors), 3.329, tolerance = 1e-3)
})

# ── S3 methods ────────────────────────────────────────────────────────────────

test_that("print.ssvkernel2d runs without error", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 30)
  expect_output(print(res), "Pilot Bandwidth X")
  expect_invisible(print(res))
})

test_that("plot.ssvkernel2d runs without error", {
  pdf(NULL)
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- ssvkernel2d(df$eruptions, df$waiting, n_grid = 30)
  expect_silent(plot(res))
  dev.off()
})
