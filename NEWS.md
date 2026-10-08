# sshist 0.2.5.1

## Bug Fixes

* `plot.sshist_2d()`: the default plot title was built with a Unicode
  multiplication sign (U+00D7). On devices using PDF-family encodings (e.g.
  `pdf()`, which `R CMD check` uses for its examples and tests) this aborted with
  `conversion failure on '...' in 'mbcsToSbcs'`. The title now uses an ASCII `x`,
  so `plot()` works on all graphics devices. This removes the only ERROR
  reported by `R CMD check`.

## Housekeeping

* `inst/WORDLIST`: added the package's technical vocabulary (`MVFFT`, `logexp`,
  `optimisation`, `parallelization`, `frontmatter`, `pts`) and dropped stale
  entries (`getOption`, typographic-apostrophe variants of `Abramson`,
  `Diaconis`, `Sturges`, `Colour`). `spelling::spell_check_package()` now
  reports no typos.

## Testing

* Test suite: 277 PASS | 0 FAIL | 0 ERROR | 0 WARN | 0 SKIP.

# sshist 0.2.5

## Bug Fixes

* `ssvkernel()`: replaced `max(256, min(raw_points, 1000))` grid formula with `min(ceil(T/dt_samp), 1000)` to match the original Python/MATLAB reference. This fixes an overly fine evaluation grid (e.g. 256 → 53 points for Old Faithful waiting data) that produced a different cost landscape and altered bandwidth estimates.

## Algorithm Improvements

### `sskernel()` / `sskernel2d()` — bandwidth optimization
* Log-spaced grid search (10 points, auto-expanding) followed by golden section refinement (up to 30 iterations) on the logexp-transformed scale. The two-stage approach combines the robustness of coarse grid search for multimodal landscapes with golden section precision — insensitive to grid resolution beyond 10 points (verified across 169 built-in R vectors/pairs + diamonds).
* 2D optimization is fully C++/OpenMP (`compute_sskernel2d_cost_cpp`, `get_tau_bounds_cpp`, `compute_kde2d_cpp`), handling isotropic Gaussian kernels with internal data standardization.

### `ssvkernel()` — gamma optimization
* Multi-start golden section search (K=5 intervals over [0, 1]) with 30 iterations per start, guaranteeing at least one cost evaluation in every GS sub-interval. The MISE cost function is implemented in C++ with fused loops (no intermediate L×L matrix allocations) and runs in parallel across the K starts via OpenMP.
* Achieves ~100% global minimum detection across 35 diverse datasets, controlled by the `ncores` parameter.
* Window functions (`Boxcar`/`Gauss`/`Laplace`/`Cauchy`) ported from R to C++ inline functions, verified against Python/MATLAB reference with `max_err ≤ 5.55e-17`.

### `sskernel()` / `sskernel2d()` / `ssvkernel()` — infrastructure
* `logexp()` / `ilogexp()` helpers extracted to `common.R` for the log-exp transformation required by golden section search.
* All four kernel functions (`boxcar`/`laplace`/`cauchy`/`gauss`) unified as C++ inline functions in `sshist_algo.cpp`, replacing separate R implementations.

## Internal Cleanup

* `R/ssvkernel.R`: removed `CostFunction()` (dead code after C++ port of gamma optimization).
* `R/common.R`: removed `kernel_boxcar()`, `kernel_laplace()`, `kernel_cauchy()`, `kernel_gauss()` (replaced by C++ inline functions).
* `R/sskernel.R`: removed Rcpp export of `test_kernels_cpp` (was used only for kernel verification during development).

## Documentation

* `README.Rmd` / `README.md`: High Performance bullet updated to reflect C++/OpenMP multi-start gamma optimisation and 2D kernel cost evaluation.
* `vignettes/introduction.Rmd`: `ncores` description now covers gamma optimisation (ssvkernel) and 2D kernel cost evaluation (sskernel2d); sskernel/sskernel2d descriptions mention two-stage grid + golden section search.
* `man/sskernel2d.Rd` / `sskernel.Rd`: description and parameters updated for the two-stage bandwidth search.

## Testing

* Updated test expectations in `test-ssvkernel.R` for the corrected grid formula (53 instead of 256 points for waiting data) and new gamma optimisation algorithm.
* Updated reference values in `test-sskernel_2d.R` and `test-ssvkernel_2d.R` following golden section refinement in sskernel2d (changed pilot bandwidths and lambda factors).

# sshist 0.2.4

## Bug Fixes

* `plot.sshist()`: added `freq = FALSE` to histogram call so the plot correctly displays density instead of frequency.

# sshist 0.2.3

## Performance Improvements

* `ssvkernel()`: replaced iterative FFT smoothing with frequency-domain windowing using `mvfft`, reducing O(M²) FFT calls to O(M) matrix operations.
* `ssvkernel()`: vectorized `which.min` lookups and precomputed distance matrices for cost evaluation.
* `ssvkernel()` `CostFunction`: fully vectorized local bandwidth selection, replacing the `for (k in 1:L)` loop with `max.col` and matrix operations.
* `ssvkernel()`: removed unnecessary `t()` transpose in MVFFT slicing and unused `weight_fun()` helper.
* `common.R`: fixed edge case in `fftkernel_1d` where `Lmax` could be zero, ensured `max(1, ...)` guard.

## Visual Improvements

* `plot.sshist()`: redesigned data display — jittered points and rug now sit in a reserved negative-y strip below the histogram bars (to avoid overlap).
* `plot.sskernel()` and `plot.ssvkernel()`: same reserved-strip approach for consistent look across all plot methods.
* All plot methods: redrawn y-axis showing only non-negative density ticks, with a subtle separator at y = 0.

## New Features

* Added iris dataset tests for all six estimators with concrete reference values (278 total tests).

## Documentation

* Added link references in README.

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
