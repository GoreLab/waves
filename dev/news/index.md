# Changelog

## waves 0.2.6.9000 (development version)

- Bug fix: Fixed namespace resolution error with
  [`lifecycle::deprecated()`](https://lifecycle.r-lib.org/reference/deprecated.html)
  function calls that was causing “could not find function deprecated”
  errors for users. All function parameters now properly use
  [`lifecycle::deprecated()`](https://lifecycle.r-lib.org/reference/deprecated.html)
  instead of `deprecated()`.

## waves 0.2.6

CRAN release: 2025-10-30

- Bug fix:
  [`plot_spectra()`](https://GoreLab.github.io/waves/dev/reference/plot_spectra.md)
  no longer returns an error when `detect.outliers` is set to `FALSE`
  and no alternative title is provided via the `alternate.title`
  parameter ([\#29](https://github.com/GoreLab/waves/issues/29)).
- Bug fix: Fixed compatibility issue with updated `spectacles` package
  (v0.5.5) that was causing data frame construction errors in model
  performance calculations.
- Fixed: Temporary CRAN archive issue with the dependency `spectacles`
  resolved ([\#31](https://github.com/GoreLab/waves/issues/31)).
  - The dependency `spectacles` is now restored on CRAN.
  - `waves` is fully compatible with the restored version.
- Performance improvements:
  - Optimized cross-validation loops in
    [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
    using vectorized indexing and preallocated result structures.
  - Optimized matrix operations in
    [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
    for faster column selection and reduced memory overhead.
  - Added shared utility functions to eliminate code duplication between
    [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
    and
    [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md).
- When `return.distances = TRUE`, the h.distance column is now located
  between metadata and spectra in the returned `data.frame`
  ([\#28](https://github.com/GoreLab/waves/issues/28)).
- Internal code improvements:
  - Significantly refactored
    [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
    and
    [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md)
    to reduce cyclomatic complexity
    ([\#26](https://github.com/GoreLab/waves/issues/26)).
    - [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
      complexity reduced from 32+ to 25
    - [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md)
      complexity reduced to 11
  - Extracted shared utility functions: `handle_deprecations()`,
    `validate_inputs()`, `partition_data()`, `train_individual_model()`,
    `calculate_performance()`, and `create_cv_control()`.
  - Improved code maintainability and reduced duplication through
    function decomposition.
  - Added robust error handling for spectacles package compatibility.

## waves 0.2.5

CRAN release: 2023-12-12

- Bug fix:
  [`predict_spectra()`](https://GoreLab.github.io/waves/dev/reference/predict_spectra.md)
  no longer returns error when running example code
  ([\#25](https://github.com/GoreLab/waves/issues/25)).

## waves 0.2.4

CRAN release: 2022-03-29

- Bug fix: Different CV schemes no longer return the same results
  ([\#20](https://github.com/GoreLab/waves/issues/20)).
- When `cv.scheme` is set to “CV2” and “CV0” and there are no
  overlapping genotypes between “trial1” and “trial2”,
  [`format_cv()`](https://GoreLab.github.io/waves/dev/reference/format_cv.md)
  now returns `NULL`. Previously, results would be returned even if no
  overlap was present, resulting in incorrect CV scheme specification.
- [`format_cv()`](https://GoreLab.github.io/waves/dev/reference/format_cv.md)
  parameter `cv.method` is now the boolean parameter
  `stratified.sampling` for consistency with other waves functions.
- [`plot_spectra()`](https://GoreLab.github.io/waves/dev/reference/plot_spectra.md)
  no longer requires a column named “unique.id”.

## waves 0.2.3

CRAN release: 2022-03-07

- Bug fix:
  [`save_model()`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  output now works correctly with
  [`predict_spectra()`](https://GoreLab.github.io/waves/dev/reference/predict_spectra.md).
- Bug fix:
  [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
  no longer returns an error when `stratified.sampling = F`.
- In
  [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md),
  stratified random sampling of training and test sets now allows the
  user to provide a seed value for
  [`set.seed()`](https://rdrr.io/r/base/Random.html). For random
  (non-stratified) sampling of training and test sets, seed is set to
  the current iteration number.
- Minor documentation updates added.

## waves 0.2.2

CRAN release: 2022-02-18

- Bug fix: `model.method = "svmLinear` and `model.method = "svmRadial`
  no longer return an error when used in
  [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
  or
  [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md).

## waves 0.2.1

CRAN release: 2022-02-03

- Bug fix:
  [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md)
  now returns trained model correctly when only one pretreatment is
  specified.
- Change the gap-segment derivative pretreatment to retain compatibility
  with prospectr. In the upcoming version of prospectr, the gapDer
  function only accepts odd values for the segment argument in order to
  properly compute the convolution filter.
- Default plot title for
  [`plot_spectra()`](https://GoreLab.github.io/waves/dev/reference/plot_spectra.md)
  is now `NULL` (no title) if `detect.outliers` is set to `FALSE`.
- Column names in output list item `$summary.model.performance` from
  [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md)
  now include underscores rather than periods for easier parsing.
- Update website
- New vignette:
  [`vignette("waves")`](https://GoreLab.github.io/waves/dev/articles/waves.md)

## waves 0.2.0

CRAN release: 2022-01-21

- Update all files to conform to the tidyverse style guide
  ([\#6](https://github.com/GoreLab/waves/issues/6)).
- All functions renamed to match tidystyle. Old functions names work
  will be dropped after this version:
  - `AggregateSpectra` -\>
    [`aggregate_spectra()`](https://GoreLab.github.io/waves/dev/reference/aggregate_spectra.md)
  - `DoPreprocessing` -\>
    [`pretreat_spectra()`](https://GoreLab.github.io/waves/dev/reference/pretreat_spectra.md)
  - `FilterSpectra` -\>
    [`filter_spectra()`](https://GoreLab.github.io/waves/dev/reference/filter_spectra.md)
  - `FormatCV`-\>
    [`format_cv()`](https://GoreLab.github.io/waves/dev/reference/format_cv.md)
  - [`PlotSpectra()`](https://GoreLab.github.io/waves/dev/reference/rename.md)-\>
    [`plot_spectra()`](https://GoreLab.github.io/waves/dev/reference/plot_spectra.md)
  - [`SaveModel()`](https://GoreLab.github.io/waves/dev/reference/rename.md)-\>
    [`save_model()`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  - [`TestModelPerformance()`](https://GoreLab.github.io/waves/dev/reference/rename.md)-\>
    [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md)
  - [`TrainSpectralModel()`](https://GoreLab.github.io/waves/dev/reference/rename.md)-\>
    [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
- “Preprocessing” has been renamed “Pretreatment” to minimize confusion
  with physical preprocessing of samples prior to scanning. Arguments
  have been renamed to reflect these changes (`preprocessing` is now
  `pretreatment`).
- Added more informative error message and documentation for random
  forest tune length (`tune.length` must be set to 5 when
  `model.algorithm == "rf"`).
- Additional flexibility for
  [`plot_spectra()`](https://GoreLab.github.io/waves/dev/reference/plot_spectra.md)
  including color and title customization and the option to forgo
  filtering ([\#5](https://github.com/GoreLab/waves/issues/5)).
- Named list output for all functions to enable easier access to
  individual elements.
- Always return model and variable importance results with
  [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
  and
  [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md).
- Add variable importance for PLSR
  ([\#9](https://github.com/GoreLab/waves/issues/9)).
- Enable selection of k for k-fold cross-validation within the training
  set. Previously, k was fixed at 5
  ([\#10](https://github.com/GoreLab/waves/issues/10)).
- [`save_model()`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  now automatically selects the best model if provided with multiple
  pretreatments.
- Code simplified and streamlined to facilitate future updates.
- Export predicted values as well as performance statistics for each
  training iteration
  ([\#11](https://github.com/GoreLab/waves/issues/11)).
- `wavelengths` is no longer a required argument for any of the *waves*
  functions.
- The proportion of samples to include in the training set can now be
  selected with the argument `proportion.train`. Previously, this
  proportion was fixed at 0.7
  ([\#13](https://github.com/GoreLab/waves/issues/13)).
- Bug fix:
  [`aggregate_spectra()`](https://GoreLab.github.io/waves/dev/reference/aggregate_spectra.md)
  now allows for aggregation by a single grouping column
  ([\#14](https://github.com/GoreLab/waves/issues/14)).
- The parameter `save.model` in the function
  [`save_model()`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  has been renamed to `write.model` for clarity.

## waves 0.1.1

CRAN release: 2021-04-21

- Bug fix: SVM Linear and SVM Radial algorithms no longer return errors
  in
  [`TrainSpectralModel()`](https://GoreLab.github.io/waves/dev/reference/rename.md).
- Bug fix: Random Forest variable importance no longer returns error in
  [`TrainSpectralModel()`](https://GoreLab.github.io/waves/dev/reference/rename.md)
  or when `preprocessing = TRUE` in
  [`TestModelPerformance()`](https://GoreLab.github.io/waves/dev/reference/rename.md)
  ([\#7](https://github.com/GoreLab/waves/issues/7)).
- Output for random forest variable importance now includes
  “Pretreatment” and “Iteration” columns.
- [`PlotSpectra()`](https://GoreLab.github.io/waves/dev/reference/rename.md)
  now allows for missing data in non-spectral columns of the input data
  frame.
- *waves* now has an [associated paper in the Plant Phenome
  Journal](https://doi.org/10.1002/ppj2.20012)! The citation for this
  paper should be used if *waves* is used in a paper - see
  citation(“waves”) for details.

## waves 0.1.0

CRAN release: 2020-09-17

Initial package release
