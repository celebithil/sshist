test_that("sshist reproduces results from the reference MATLAB script (Old Faithful subset)", {

  # 1. Data from Shimazaki examples (107 observations)
  x <- c(4.37, 3.87, 4.00, 4.03, 3.50, 4.08, 2.25, 4.70, 1.73, 4.93, 1.73, 4.62,
         3.43, 4.25, 1.68, 3.92, 3.68, 3.10, 4.03, 1.77, 4.08, 1.75, 3.20, 1.85,
         4.62, 1.97, 4.50, 3.92, 4.35, 2.33, 3.83, 1.88, 4.60, 1.80, 4.73, 1.77,
         4.57, 1.85, 3.52, 4.00, 3.70, 3.72, 4.25, 3.58, 3.80, 3.77, 3.75, 2.50,
         4.50, 4.10, 3.70, 3.80, 3.43, 4.00, 2.27, 4.40, 4.05, 4.25, 3.33, 2.00,
         4.33, 2.93, 4.58, 1.90, 3.58, 3.73, 3.73, 1.82, 4.63, 3.50, 4.00, 3.67,
         1.67, 4.60, 1.67, 4.00, 1.80, 4.42, 1.90, 4.63, 2.93, 3.50, 1.97, 4.28,
         1.83, 4.13, 1.83, 4.65, 4.20, 3.93, 4.33, 1.83, 4.53, 2.03, 4.18, 4.43,
         4.07, 4.13, 3.95, 4.10, 2.27, 4.58, 1.90, 4.50, 1.95, 4.83, 4.12)

  # 2. Run the algorithm
  # Use sn=30 as in the default MATLAB script
  res <- sshist(x, sn = 30)

  # 3. Checks

  # Class check
  expect_s3_class(res, "sshist")

  # "Gold Standard" check (should match sshist_v2.m)
  # We expect 12, as our algorithm uses Shift-Averaging
  expect_equal(res$opt_n, 12, info = "The algorithm should select 12 bins for this dataset")

  # Boundaries check (edges should cover min and max)
  expect_lte(min(res$edges), min(x))
  expect_gte(max(res$edges), max(x))

  # Cost function integrity check
  # The cost vector should be the same length as the tested N values
  expect_equal(length(res$cost), length(res$n_tested))

  # Check that we found the minimum (or close to it)
  # The cost value at opt_n should be the minimum in the cost vector
  min_cost_idx <- which.min(res$cost)
  expect_equal(res$n_tested[min_cost_idx], res$opt_n)
})
