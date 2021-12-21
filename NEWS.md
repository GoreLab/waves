# waves 0.1.1.9000

* Added more informative error message and documentation for random forest tune length (tune.length must be set to 5 when model.algorithm == "rf").
* Additional flexibility for `PlotSpectra()` including color and title customization and the option to forgo filtering.
* Named list output for all functions to enable easier access to individual elements.
* Always return model and variable importance results with `TrainSpectralModel()` and `TestModelPerformance()`.
* Add variable importance for PLSR (#9).
* Enable selection of k for k-fold cross-validation within the training set. Previously, k was fixed at 5 (#10).
* "Preprocessing" has been renamed "Pretreatment" to minimize confusion with physical preprocessing of samples prior to scanning. Arguments have been renamed to reflect these changes (`preprocessing` is now `pretreatment`).
* `SaveModel()` now automatically selects the best model if provided with multiple pretreatments.
* Code simplified and streamlined to facilitate future updates.
* Export predicted values as well as performance statistics for each training iteration (#11).
* `wavelengths` is no longer a required argument for any of the *waves* functions.

# waves 0.1.1

* Bug fix: SVM Linear and SVM Radial algorithms no longer return errors in `TrainSpectralModel()`.
* Bug fix: Random Forest variable importance no longer returns error in `TrainSpectralModel()` or when `preprocessing = TRUE` in `TestModelPerformance()` (#7). 
* Output for random forest variable importance now includes "Pretreatment" and "Iteration" columns.
* `PlotSpectra()` now allows for missing data in non-spectral columns of the input data frame.
* *waves* now has an [associated paper in the Plant Phenome Journal](https://doi.org/10.1002/ppj2.20012)! The citation for this paper should be used if *waves* is used in a paper - see citation("waves") for details.

# waves 0.1.0

Initial package release
