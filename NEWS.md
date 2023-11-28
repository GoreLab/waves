# waves 0.2.5

* Bug fix: `predict_spectra()` no longer returns error when running example code (#25).


# waves 0.2.4

* Bug fix: Different CV schemes no longer return the same results (#20).
* When `cv.scheme` is set to "CV2" and "CV0" and there are no overlapping genotypes between "trial1" and "trial2", `format_cv()` now returns `NULL`. Previously, results would be returned even if no overlap was present, resulting in incorrect CV scheme specification.
* `format_cv()` parameter `cv.method` is now the boolean parameter `stratified.sampling` for consistency with other waves functions. 
* `plot_spectra()` no longer requires a column named "unique.id".


# waves 0.2.3

* Bug fix: `save_model()` output now works correctly with `predict_spectra()`.
* Bug fix: `train_spectra()` no longer returns an error when `stratified.sampling = F`.
* In `train_spectra()`, stratified random sampling of training and test sets now allows the user to provide a seed value for `set.seed()`. For random (non-stratified) sampling of training and test sets, seed is set to the current iteration number.
* Minor documentation updates added.


# waves 0.2.2

* Bug fix: `model.method = "svmLinear` and `model.method = "svmRadial` no longer return an error when used in `train_spectra()` or `test_spectra()`.


# waves 0.2.1

* Bug fix: `test_spectra()` now returns trained model correctly when only one pretreatment is specified.
* Change the gap-segment derivative pretreatment to retain compatibility with prospectr. In the upcoming version of prospectr, the gapDer function only accepts odd values for the segment argument in order to properly compute the convolution filter.
* Default plot title for `plot_spectra()` is now `NULL` (no title) if `detect.outliers` is set to `FALSE`.
* Column names in output list item `$summary.model.performance` from `test_spectra()` now include underscores rather than periods for easier parsing.
* Update website
* New vignette: `vignette("waves")`


# waves 0.2.0

* Update all files to conform to the tidyverse style guide (#6).
* All functions renamed to match tidystyle. Old functions names work will be dropped after this version: 
    - `AggregateSpectra` -> `aggregate_spectra()`
    - `DoPreprocessing` -> `pretreat_spectra()`
    - `FilterSpectra` -> `filter_spectra()`
    - `FormatCV`-> `format_cv()`
    - `PlotSpectra()`-> `plot_spectra()`
    - `SaveModel()`-> `save_model()`
    - `TestModelPerformance()`-> `test_spectra()`
    - `TrainSpectralModel()`-> `train_spectra()`
* "Preprocessing" has been renamed "Pretreatment" to minimize confusion with physical preprocessing of samples prior to scanning. Arguments have been renamed to reflect these changes (`preprocessing` is now `pretreatment`).
* Added more informative error message and documentation for random forest tune length (`tune.length` must be set to 5 when `model.algorithm == "rf"`).
* Additional flexibility for `plot_spectra()` including color and title customization and the option to forgo filtering (#5).
* Named list output for all functions to enable easier access to individual elements.
* Always return model and variable importance results with `train_spectra()` and `test_spectra()`.
* Add variable importance for PLSR (#9).
* Enable selection of k for k-fold cross-validation within the training set. Previously, k was fixed at 5 (#10).
* `save_model()` now automatically selects the best model if provided with multiple pretreatments.
* Code simplified and streamlined to facilitate future updates.
* Export predicted values as well as performance statistics for each training iteration (#11).
* `wavelengths` is no longer a required argument for any of the *waves* functions.
* The proportion of samples to include in the training set can now be selected with the argument `proportion.train`. Previously, this proportion was fixed at 0.7 (#13).
* Bug fix: `aggregate_spectra()` now allows for aggregation by a single grouping column (#14).
* The parameter `save.model` in the function `save_model()` has been renamed to `write.model` for clarity.


# waves 0.1.1

* Bug fix: SVM Linear and SVM Radial algorithms no longer return errors in `TrainSpectralModel()`.
* Bug fix: Random Forest variable importance no longer returns error in `TrainSpectralModel()` or when `preprocessing = TRUE` in `TestModelPerformance()` (#7). 
* Output for random forest variable importance now includes "Pretreatment" and "Iteration" columns.
* `PlotSpectra()` now allows for missing data in non-spectral columns of the input data frame.
* *waves* now has an [associated paper in the Plant Phenome Journal](https://doi.org/10.1002/ppj2.20012)! The citation for this paper should be used if *waves* is used in a paper - see citation("waves") for details.

# waves 0.1.0

Initial package release
