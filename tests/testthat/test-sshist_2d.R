options(sshist.ncores = 2)

# ── Reference value tests ─────────────────────────────────────────────────────

test_that("sshist_2d reproduces reference values on oldfaithful", {
  df  <- read.table("oldfaithful.txt", header = FALSE, col.names = c("eruptions", "waiting"))
  res <- sshist_2d(df$eruptions, df$waiting)
  expect_s3_class(res, "sshist_2d")
  expect_equal(res$opt_nx, 20L)
  expect_equal(res$opt_ny, 9L)
  expect_equal(res$opt_dx, 0.175, tolerance = 1e-6)
  expect_equal(res$opt_dy, 5.8888889, tolerance = 1e-6)
})

# ── Input flexibility ─────────────────────────────────────────────────────────

test_that("sshist_2d accepts two vectors, matrix, and data.frame", {
  x <- faithful$eruptions
  y <- faithful$waiting

  r1 <- sshist_2d(x, y)
  r2 <- sshist_2d(cbind(x, y))
  r3 <- sshist_2d(data.frame(x, y))

  expect_equal(r1$opt_nx, r2$opt_nx)
  expect_equal(r1$opt_nx, r3$opt_nx)
  expect_equal(r1$opt_ny, r2$opt_ny)
})

# ── Input validation ──────────────────────────────────────────────────────────

test_that("sshist_2d errors on non-numeric input", {
  expect_error(suppressWarnings(sshist_2d(letters, 1:10)))
  expect_error(suppressWarnings(sshist_2d(1:10, letters)))
})

test_that("sshist_2d errors with fewer than 2 complete observations", {
  expect_error(sshist_2d(1, 2))
  expect_error(sshist_2d(1:2, NA))
})

test_that("sshist_2d errors with mismatched vector lengths", {
  expect_error(sshist_2d(1:10, 1:5))
})

test_that("sshist_2d errors on constant data", {
  expect_error(sshist_2d(rep(5, 10), 1:10), "constant")
  expect_error(sshist_2d(1:10, rep(5, 10)), "constant")
})

test_that("sshist_2d errors with invalid matrix input", {
  expect_error(sshist_2d(matrix(1:9, ncol = 3)), "exactly 2 columns")
  expect_error(sshist_2d(list(a = 1:5, b = 1:5)))
})

test_that("sshist_2d removes NAs silently", {
  x <- c(1:20, NA, NA)
  y <- c(21:40, NA, NA)
  res <- sshist_2d(x, y)
  expect_equal(length(res$data$x), 20L)
  expect_equal(length(res$data$y), 20L)
})

# ── Parameter variations ──────────────────────────────────────────────────────

test_that("sshist_2d respects n_min and n_max", {
  x <- faithful$eruptions
  y <- faithful$waiting
  res <- sshist_2d(x, y, n_min = 5, n_max = 10)
  expect_gte(res$opt_nx, 5)
  expect_gte(res$opt_ny, 5)
  expect_lte(res$opt_nx, 10)
  expect_lte(res$opt_ny, 10)
})

# ── Return value structure ────────────────────────────────────────────────────

test_that("sshist_2d returns correct structure", {
  res <- sshist_2d(faithful$eruptions, faithful$waiting)
  expect_named(res, c("opt_nx", "opt_ny", "opt_dx", "opt_dy", "data"))
  expect_type(res$opt_nx, "integer")
  expect_type(res$opt_ny, "integer")
  expect_type(res$opt_dx, "double")
  expect_type(res$opt_dy, "double")
  expect_true(all(res$opt_dx > 0, res$opt_dy > 0))
})

# ── S3 methods ────────────────────────────────────────────────────────────────

test_that("print.sshist_2d runs without error", {
  res <- sshist_2d(faithful$eruptions, faithful$waiting)
  expect_output(print(res), "Optimal Bins X")
  expect_invisible(print(res))
})

test_that("plot.sshist_2d runs without error", {
  pdf(NULL)
  res <- sshist_2d(faithful$eruptions, faithful$waiting)
  expect_silent(plot(res))
  dev.off()
})
