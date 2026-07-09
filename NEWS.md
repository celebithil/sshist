# sshist 0.2.2

## CRAN Compliance Fixes

* Removed invalid `\cr` from `\describe{}` in package documentation, fixing "LaTeX Error: There's no line here to end."
* Quoted `OpenMP` and `backends` in DESCRIPTION to avoid spelling NOTE on CRAN.

# sshist 0.2.1

## README Fixes

* Removed YAML frontmatter (`output: github_document`) from `README.Rmd` and `README.md`.

# sshist 0.2.0

## New Features

* Added `sskernel()` for optimal 1D fixed-bandwidth kernel density estimation.
* Added `sskernel2d()` for optimal 2D fixed-bandwidth kernel density estimation.
* Added `ssvkernel()` for locally adaptive 1D kernel density estimation (Shimazaki & Shinomoto 2010).
* Added `ssvkernel2d()` for locally adaptive 2D kernel density estimation (Abramson's method).
* Added bootstrap confidence interval support for all kernel density estimators.
* Added C++ backends for 2D KDE cost computation, pilot density, and grid evaluation with OpenMP parallelism.
* Added `ncores` parameter to `sshist()` for multithreaded computation.

## Algorithm Improvements

* Re-implemented `sshist()` with cleaner exhaustive search logic, exactly matching the original Python/MATLAB reference algorithms.
* Added resolution guard (anti-comb effect) with `N_max = Range / (2 * Min_Resolution)`.
* Improved 2D histogram cost computation with pre-computed Y-bin indices for significant speedup.
* Added auto-expanding grid search for kernel bandwidth optimization.

## Documentation

* Added vignettes: "Introduction to sshist" and "ggplot2 Visualization".
* Added comprehensive S3 plot/print methods for all estimator classes.
* Updated README with complete function summary table.

## Internal Changes

* Split code into modular R files: `sshist.R`, `sskernel.R`, `ssvkernel.R`, `common.R`.
* Updated to roxygen2 8.0 / `Config/roxygen2/version` format.
* Removed `cost` and `n_tested` fields from `sshist` return value (simplified output).
* Added C++ Rcpp functions: `get_tau_bounds_cpp`, `compute_sskernel2d_cost_cpp`, `compute_pilot_density_cpp`, `compute_kde2d_cpp`.

# sshist 0.1.3

## Algorithm Improvements

*   Updated the binning resolution limit formula to `N_max = Range / (2 * Min_Resolution)` to prevent the "comb effect" (sampling artifacts). This change aligns the R implementation with the author's reference code.
*   The `n_max` parameter in `sshist()` is now strictly bounded by the resolution limit to prevent overfitting.

## Documentation

* Fixed formatting in the `DESCRIPTION` file.
* Updated the `faithful` dataset examples in `README.md` to reflect the corrected optimal bin calculations (N changed from 37 to 21).
* Added CRAN download badges to `README.md`.
* Expanded reference links in `README.md` to include the original toolboxes and GitHub repositories from the algorithm's authors.


# sshist 0.1.2

## CRAN Submission Fixes

* Fixed DESCRIPTION file reference formatting:
  - Added author names (Shimazaki and Shinomoto) to citation
  - Wrapped 'C++' in single quotes to comply with CRAN formatting requirements

* Improved documentation:
  - Added `\value` tags to all exported S3 method documentation files
  - Added comprehensive descriptions of return values and side effects for plot and print methods
  - Updated print methods to return objects invisibly following R best practices
  - Clear old images

* Fixed vignette:
  - Corrected graphical parameter handling in introduction.Rmd
  - Now properly stores and restores user's `par()` settings

## Internal Changes

* Added `invisible(x)` return statements to `print.sshist()` and `print.sshist_2d()` methods


# sshist 0.1.1
## Cosmetic fixes
* Fix installation links at Readme.Rmd.


# sshist 0.1.0
* Initial CRAN submission.
* Added `sshist()` for 1D optimization (C++ optimized).
* Added `sshist_2d()` for 2D optimization.
* Added support for `ggplot2` integration in examples.
