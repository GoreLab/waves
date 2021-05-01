# waves 0.1.1.9000

* Added more informative error message and documentation for random forest tune length (tune.length must be set to 5).

# waves 0.1.1

* Bug fix: SVM Linear and SVM Radial algorithms no longer return errors in `TrainSpectralModel()`.
* Bug fix: Random Forest variable importance no longer returns error in `TrainSpectralModel()` or when `preprocessing = TRUE` in `TestModelPerformance()` (#7). 
* Output for random forest variable importance now includes "Pretreatment" and "Iteration" columns.
* `PlotSpectra()` now allows for missing data in non-spectral columns of the input data frame.
* *waves* now has an [associated paper in the Plant Phenome Journal](https://doi.org/10.1002/ppj2.20012)! The citation for this paper should be used if *waves* is used in a paper - see citation("waves") for details.

# waves 0.1.0

Initial package release
