# Train a model based predict reference values with spectral data

Trains spectral prediction models using one of several algorithms and
sampling procedures.

## Usage

``` r
train_spectra(
  df,
  num.iterations,
  test.data = NULL,
  k.folds = 5,
  proportion.train = 0.7,
  tune.length = 50,
  model.method = "pls",
  best.model.metric = "RMSE",
  stratified.sampling = TRUE,
  cv.scheme = NULL,
  trial1 = NULL,
  trial2 = NULL,
  trial3 = NULL,
  split.test = FALSE,
  seed = 1,
  verbose = TRUE,
  save.model = lifecycle::deprecated(),
  rf.variable.importance = lifecycle::deprecated(),
  output.summary = lifecycle::deprecated(),
  return.model = lifecycle::deprecated()
)
```

## Arguments

- df:

  `data.frame` object. First column contains unique identifiers, second
  contains reference values, followed by spectral columns. Include no
  other columns to right of spectra! Column names of spectra must start
  with "X" and reference column must be named "reference"

- num.iterations:

  Number of training iterations to perform

- test.data:

  `data.frame` with same specifications as `df`. Use if specific test
  set is desired for hyperparameter tuning. If `NULL`, function will
  automatically train with a stratified sample of 70%. Default is
  `NULL`.

- k.folds:

  Number indicating the number of folds for k-fold cross-validation
  during model training. Default is 5.

- proportion.train:

  Fraction of samples to include in the training set. Default is 0.7.

- tune.length:

  Number delineating search space for tuning of the PLSR hyperparameter
  `ncomp`. Must be set to 5 when using the random forest algorithm
  (`model.method == rf`). Default is 50.

- model.method:

  Model type to use for training. Valid options include:

  - "pls": Partial least squares regression (Default)

  - "rf": Random forest

  - "svmLinear": Support vector machine with linear kernel

  - "svmRadial": Support vector machine with radial kernel

- best.model.metric:

  Metric used to decide which model is best. Must be either "RMSE" or
  "Rsquared"

- stratified.sampling:

  If `TRUE`, training and test sets will be selected using stratified
  random sampling. This term is only used if `test.data == NULL`.
  Default is `TRUE`.

- cv.scheme:

  A cross validation (CV) scheme from Jarquín et al., 2017. Options for
  `cv.scheme` include:

  - "CV1": untested lines in tested environments

  - "CV2": tested lines in tested environments

  - "CV0": tested lines in untested environments

  - "CV00": untested lines in untested environments

- trial1:

  `data.frame` object that is for use only when `cv.scheme` is provided.
  Contains the trial to be tested in subsequent model training
  functions. The first column contains unique identifiers, second
  contains genotypes, third contains reference values, followed by
  spectral columns. Include no other columns to right of spectra! Column
  names of spectra must start with "X", reference column must be named
  "reference", and genotype column must be named "genotype".

- trial2:

  `data.frame` object that is for use only when `cv.scheme` is provided.
  This data.frame contains a trial that has overlapping genotypes with
  `trial1` but that were grown in a different site/year (different
  environment). Formatting must be consistent with `trial1`.

- trial3:

  `data.frame` object that is for use only when `cv.scheme` is provided.
  This data.frame contains a trial that may or may not contain genotypes
  that overlap with `trial1`. Formatting must be consistent with
  `trial1`.

- split.test:

  boolean that allows for a fixed training set and a split test set.
  Example// train model on data from two breeding programs and a
  stratified subset (70%) of a third and test on the remaining samples
  (30%) of the third. If `FALSE`, the entire provided test set
  `test.data` will remain as a testing set or if none is provided, 30%
  of the provided `train.data` will be used for testing. Default is
  `FALSE`.

- seed:

  Integer to be used internally as input for
  [`set.seed()`](https://rdrr.io/r/base/Random.html). Only used if
  `stratified.sampling = TRUE`. In all other cases, seed is set to the
  current iteration number. Default is 1.

- verbose:

  If `TRUE`, the number of rows removed through filtering will be
  printed to the console. Default is `TRUE`.

- save.model:

  DEPRECATED `save.model = FALSE` is no longer supported; this function
  will always return a saved model.

- rf.variable.importance:

  DEPRECATED `rf.variable.importance = FALSE` is no longer supported;
  variable importance results are always returned if the `model.method`
  is set to \`pls\` or \`rf\`.

- output.summary:

  DEPRECATED `output.summary = FALSE` is no longer supported; a summary
  of output is always returned alongside the full performance
  statistics.

- return.model:

  DEPRECATED `return.model = FALSE` is no longer supported; a trained
  model object is always returned alongside the full performance
  statistics and summary.

## Value

list of the following:

1.  `model` is a model object trained with all rows of `df`.

2.  `summary.model.performance` is a `data.frame` with model performance
    statistics in summary format (2 rows, one with mean and one with
    standard deviation of all training iterations).

3.  `full.model.performance` is a `data.frame` with model performance
    statistics in long format (number of rows = `num.iterations`)

4.  `predictions` is a `data.frame` containing predicted values for each
    test set entry at each iteration of model training.

5.  `importance` is a `data.frame` that contains variable importance for
    each wavelength. Only available for `model.method` options "rf" and
    "pls".

Included summary statistics:

- Tuned parameters depending on the model algorithm:

  - **Best.n.comp**, the best number of components

  - **Best.ntree**, the best number of trees in an RF model

  - **Best.mtry**, the best number of variables to include at every
    decision point in an RF model

- **RMSECV**, the root mean squared error of cross-validation

- **R2cv**, the coefficient of multiple determination of
  cross-validation for PLSR models

- **RMSEP**, the root mean squared error of prediction

- **R2p**, the squared Pearson’s correlation between predicted and
  observed test set values

- **RPD**, the ratio of standard deviation of observed test set values
  to RMSEP

- **RPIQ**, the ratio of performance to interquartile difference

- **CCC**, the concordance correlation coefficient

- **Bias**, the average difference between the predicted and observed
  values

- **SEP**, the standard error of prediction

- **R2sp**, the squared Spearman’s rank correlation between predicted
  and observed test set values

## Author

Jenna Hershberger <jmh579@cornell.edu>

## Examples

``` r
# \donttest{
library(magrittr)
ikeogu.2017 %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::rename(reference = DMC.oven,
                unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  train_spectra(
    df = .,
    tune.length = 3,
    num.iterations = 3,
    best.model.metric = "RMSE",
    stratified.sampling = TRUE
  ) %>%
  summary()
#> Returning model...
#>                           Length Class      Mode
#> model                     20     mvr        list
#> summary.model.performance 15     data.frame list
#> model.performance         15     data.frame list
#> predictions                5     data.frame list
#> importance                 4     data.frame list
# }
```
