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
