## Test environments
* local Apple Silicon (M1), macOS 15.6.1 Ventura, R 4.3.3
* GitHub Actions CI:
  - ubuntu-24.04 (release, devel)
  - windows-latest (release)
  - macOS-latest (release)
* R-hub
  - linux (R-devel)
  - macos (R-devel)
  - windows (R-devel)

## R CMD check results
0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

This note is unrelated to the package itself and occurs when system time verification fails during the check process on my local installation.

## Comments
This is a maintenance release that:
* Fixes compatibility issue with updated spectacles package (v0.5.5) that was causing runtime errors
* Significantly improves performance of train_spectra() and test_spectra() through optimized algorithms and reduced memory usage
* Reduces cyclomatic complexity of core functions through internal refactoring
* Eliminates code duplication with shared utility functions
* Adds robust error handling for better package reliability
* Introduces no user-facing API changes (all existing code remains fully compatible)

This release focuses on code quality improvements, performance optimization, and bug fixes while maintaining complete backwards compatibility.

## Downstream dependencies
There are currently no downstream dependencies for this package.
