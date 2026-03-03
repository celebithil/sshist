# sshist 0.1.3

## Algorithm Improvements

*   Updated the binning resolution limit formula to `N_max = Range / (2 * Min_Resolution)` to prevent the "comb effect" (sampling artifacts). This change aligns the R implementation with the author's reference Python logic and FAQ.
*   The `n_max` parameter in `sshist()` is now strictly bounded by the resolution limit to prevent overfitting.

## Documentation & Metadata

*   Updated `README.md` and `README.Rmd` with new references and CRAN download badges.
*   Adjusted the example output in the README to reflect the new algorithm logic.
*   Updated `DESCRIPTION` file with refined URL indentation for better compliance.

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
