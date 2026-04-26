options(sshist.ncores = 2)

# ── Input flexibility ─────────────────────────────────────────────────────────

test_that("sskernel2d accepts two vectors and matrix inputs identically", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  x <- df$eruptions; y <- df$waiting

  rv <- sskernel2d(x, y, n_grid = 50L)
  rm <- sskernel2d(cbind(x, y), n_grid = 50L)

  expect_s3_class(rv, "sskernel2d")
  expect_s3_class(rm, "sskernel2d")
  expect_equal(rv$opt_wx, rm$opt_wx)
  expect_equal(rv$opt_wy, rm$opt_wy)
  expect_equal(rv$z, rm$z)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("sskernel2d errors on non-numeric input", {
  expect_error(suppressWarnings(sskernel2d(letters, 1:10)))
  expect_error(suppressWarnings(sskernel2d(1:10, letters)))
})

test_that("sskernel2d errors with insufficient data", {
  expect_error(sskernel2d(1, 2))
})

test_that("sskernel2d silently removes NAs", {
  x <- c(1:20, NA, NA); y <- c(21:40, NA, NA)
  res <- sskernel2d(x, y, n_grid = 10)
  expect_s3_class(res, "sskernel2d")
})

# ── Parameter variations ──────────────────────────────────────────────────────

test_that("sskernel2d output dimensions match n_grid", {
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  for (ng in c(20L, 50L, 80L)) {
    res <- sskernel2d(df$eruptions, df$waiting, n_grid = ng)
    expect_length(res$x_grid, ng)
    expect_length(res$y_grid, ng)
    expect_equal(dim(res$z), c(ng, ng))
  }
})

test_that("sskernel2d accepts user-supplied W grid", {
  df <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  W  <- seq(0.05, 1, length.out = 15)
  res <- sskernel2d(df$eruptions, df$waiting, W = W, n_grid = 30)
  expect_s3_class(res, "sskernel2d")
  expect_true(res$opt_wx > 0)
})

# ── Mathematical properties ───────────────────────────────────────────────────

test_that("sskernel2d density is non-negative and integrates approx to 1", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel2d(df$eruptions, df$waiting, n_grid = 80L)
  expect_true(res$opt_wx > 0 && res$opt_wy > 0)
  expect_true(all(res$z >= 0))
  dx <- min(diff(res$x_grid)); dy <- min(diff(res$y_grid))
  expect_equal(sum(res$z) * dx * dy, 1.0, tolerance = 5e-2)
})

# ── Reference values ──────────────────────────────────────────────────────────

test_that("sskernel2d reproduces reference regression values", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel2d(df$eruptions, df$waiting, n_grid = 100L)
  expect_equal(res$opt_wx, 0.1947, tolerance = 1e-3)
  expect_equal(res$opt_wy, 2.319,  tolerance = 1e-3)
  expect_equal(max(res$z), 0.0405, tolerance = 1e-3)
  expect_equal(min(res$z), 0,      tolerance = 1e-3)
})

# ── Return value structure ────────────────────────────────────────────────────

test_that("sskernel2d returns correct structure", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel2d(df$eruptions, df$waiting, n_grid = 30)
  expect_named(res, c("x_grid", "y_grid", "z", "opt_wx", "opt_wy", "data"))
  expect_type(res$opt_wx, "double")
  expect_type(res$opt_wy, "double")
})

# ── S3 methods ────────────────────────────────────────────────────────────────

test_that("print.sskernel2d runs without error", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel2d(df$eruptions, df$waiting, n_grid = 30)
  expect_output(print(res), "Optimal Bandwidth X")
  expect_invisible(print(res))
})

test_that("plot.sskernel2d runs without error", {
  pdf(NULL)
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sskernel2d(df$eruptions, df$waiting, n_grid = 30)
  expect_silent(plot(res))
  dev.off()
})
