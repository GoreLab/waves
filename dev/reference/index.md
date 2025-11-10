# Package index

## Core model development workflow

These functions prepare spectra for model development and allow for the
streamlined testing of model performance.

- [`plot_spectra()`](https://GoreLab.github.io/waves/dev/reference/plot_spectra.md)
  : Plot spectral data, highlighting outliers as identified using
  Mahalanobis distance
- [`filter_spectra()`](https://GoreLab.github.io/waves/dev/reference/filter_spectra.md)
  : Filter spectral data frame based on Mahalanobis distance
- [`aggregate_spectra()`](https://GoreLab.github.io/waves/dev/reference/aggregate_spectra.md)
  : Aggregate data based on grouping variables and a user-provided
  function
- [`test_spectra()`](https://GoreLab.github.io/waves/dev/reference/test_spectra.md)
  : Test the performance of spectral models

## Internal use

While these functions can be used on their own, they were developed to
be called within test_spectra().

- [`pretreat_spectra()`](https://GoreLab.github.io/waves/dev/reference/pretreat_spectra.md)
  : Pretreat spectral data according to user-designated method
- [`format_cv()`](https://GoreLab.github.io/waves/dev/reference/format_cv.md)
  : Format multiple trials with or without overlapping genotypes into
  training and test sets according to user-provided cross validation
  scheme
- [`train_spectra()`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
  : Train a model based predict reference values with spectral data

## Model development and testing for routine use

- [`save_model()`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  : Save spectral prediction model and model performance statistics
- [`predict_spectra()`](https://GoreLab.github.io/waves/dev/reference/predict_spectra.md)
  : Use provided model object to predict trait values with input dataset

## Example Dataset

- [`ikeogu.2017`](https://GoreLab.github.io/waves/dev/reference/ikeogu.2017.md)
  : Example vis-NIRS and reference dataset
