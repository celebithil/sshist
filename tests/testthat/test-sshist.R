options(sshist.ncores = 2)

# ── Reference value tests ─────────────────────────────────────────────────────

test_that("sshist reproduces results from the reference MATLAB script (Old Faithful subset)", {
  x <- c(4.37, 3.87, 4.00, 4.03, 3.50, 4.08, 2.25, 4.70, 1.73, 4.93, 1.73, 4.62,
         3.43, 4.25, 1.68, 3.92, 3.68, 3.10, 4.03, 1.77, 4.08, 1.75, 3.20, 1.85,
         4.62, 1.97, 4.50, 3.92, 4.35, 2.33, 3.83, 1.88, 4.60, 1.80, 4.73, 1.77,
         4.57, 1.85, 3.52, 4.00, 3.70, 3.72, 4.25, 3.58, 3.80, 3.77, 3.75, 2.50,
         4.50, 4.10, 3.70, 3.80, 3.43, 4.00, 2.27, 4.40, 4.05, 4.25, 3.33, 2.00,
         4.33, 2.93, 4.58, 1.90, 3.58, 3.73, 3.73, 1.82, 4.63, 3.50, 4.00, 3.67,
         1.67, 4.60, 1.67, 4.00, 1.80, 4.42, 1.90, 4.63, 2.93, 3.50, 1.97, 4.28,
         1.83, 4.13, 1.83, 4.65, 4.20, 3.93, 4.33, 1.83, 4.53, 2.03, 4.18, 4.43,
         4.07, 4.13, 3.95, 4.10, 2.27, 4.58, 1.90, 4.50, 1.95, 4.83, 4.12)
  res <- sshist(x, sn = 30)
  expect_s3_class(res, "sshist")
  expect_equal(res$opt_n, 12L)
  expect_lte(min(res$edges), min(x))
  expect_gte(max(res$edges), max(x))
})

test_that("sshist reproduces reference Python values for waiting", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sshist(df$waiting)
  expect_equal(res$opt_n, 21L)
  expect_equal(res$opt_d, 2.5238095238, tolerance = 1e-6)
  expect_equal(min(res$edges), 43.0)
  expect_equal(max(res$edges), 96.0)
})

test_that("sshist reproduces reference Python values for eruptions", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sshist(df$eruptions)
  expect_equal(res$opt_n, 20L)
  expect_equal(res$opt_d, 0.1750, tolerance = 1e-6)
  expect_equal(min(res$edges), 1.60)
  expect_equal(max(res$edges), 5.10)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("sshist errors on non-numeric input", {
  expect_error(suppressWarnings(sshist(letters)), "numeric")
})

test_that("sshist errors with fewer than 2 non-missing points", {
  expect_error(suppressWarnings(sshist(numeric(0))))
  expect_error(sshist(1))
  expect_error(sshist(c(NA, NA)))
})

test_that("sshist errors on constant data", {
  expect_error(sshist(rep(5, 10)), "constant")
})

test_that("sshist silently removes NAs", {
  res <- sshist(c(1:10, NA, NA, 11:20))
  expect_s3_class(res, "sshist")
  expect_equal(length(res$data), 20L)
})

test_that("sshist errors with invalid n_max", {
  expect_error(sshist(1:10, n_max = -1))
  expect_error(sshist(1:10, n_max = "a"))
})

# ── Parameter variations ──────────────────────────────────────────────────────

test_that("sshist respects n_max parameter", {
  res <- sshist(faithful$waiting, n_max = 5)
  expect_lte(res$opt_n, 5)
})

test_that("sshist works with different shift counts", {
  res1 <- sshist(faithful$waiting, sn = 1)
  res2 <- sshist(faithful$waiting, sn = 50)
  expect_s3_class(res1, "sshist")
  expect_s3_class(res2, "sshist")
})

test_that("sshist works with ncores parameter", {
  res <- sshist(faithful$waiting, ncores = 2)
  expect_s3_class(res, "sshist")
  expect_equal(res$opt_n, 21L)
})

# ── Return value structure ────────────────────────────────────────────────────

test_that("sshist returns correct structure", {
  res <- sshist(faithful$waiting)
  expect_named(res, c("opt_n", "opt_d", "edges", "data"))
  expect_type(res$opt_n, "integer")
  expect_type(res$opt_d, "double")
  expect_type(res$edges, "double")
  expect_type(res$data, "double")
  expect_equal(length(res$edges), res$opt_n + 1L)
})

# ── S3 methods ────────────────────────────────────────────────────────────────

test_that("print.sshist runs without error", {
  res <- sshist(faithful$waiting)
  expect_output(print(res), "Optimal Bins")
  expect_invisible(print(res))
})

test_that("plot.sshist runs without error", {
  pdf(NULL)
  res <- sshist(faithful$waiting)
  expect_silent(plot(res))
  dev.off()
})

# ── Iris dataset ─────────────────────────────────────────────────────────────

test_that("sshist returns expected values for iris columns", {
  res <- sshist(iris$Sepal.Length)
  expect_equal(res$opt_n, 15L)
  expect_equal(res$opt_d, 0.24, tolerance = 1e-6)

  res <- sshist(iris$Sepal.Width)
  expect_equal(res$opt_n, 10L)
  expect_equal(res$opt_d, 0.24, tolerance = 1e-6)

  res <- sshist(iris$Petal.Length)
  expect_equal(res$opt_n, 25L)
  expect_equal(res$opt_d, 0.236, tolerance = 1e-3)

  res <- sshist(iris$Petal.Width)
  expect_equal(res$opt_n, 12L)
  expect_equal(res$opt_d, 0.2, tolerance = 1e-6)
})
