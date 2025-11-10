# Save spectral prediction model and model performance statistics

Given a set of pretreatment methods, saves the best spectral prediction
model and model statistics to `model.save.folder` as `model.name.Rds`
and `model.name_stats.csv` respectively. If only one pretreatment method
is supplied, results from that method are stored.

## Usage

``` r
save_model(
  df,
  write.model = TRUE,
  pretreatment = 1,
  model.save.folder = NULL,
  model.name = "PredictionModel",
  best.model.metric = "RMSE",
  k.folds = 5,
  proportion.train = 0.7,
  tune.length = 50,
  model.method = "pls",
  num.iterations = 10,
  stratified.sampling = TRUE,
  cv.scheme = NULL,
  trial1 = NULL,
  trial2 = NULL,
  trial3 = NULL,
  seed = 1,
  verbose = TRUE,
  save.model = lifecycle::deprecated(),
  wavelengths = lifecycle::deprecated(),
  autoselect.preprocessing = lifecycle::deprecated(),
  preprocessing.method = lifecycle::deprecated()
)
```

## Arguments

- df:

  `data.frame` object. First column contains unique identifiers, second
  contains reference values, followed by spectral columns. Include no
  other columns to right of spectra! Column names of spectra must start
  with "X" and reference column must be named "reference"

- write.model:

  If `TRUE`, the trained model will be saved in .Rds format to the
  location specified by `model.save.folder`. If `FALSE`, the best model
  will be output by the function but will not save to a file. Default is
  `TRUE`.

- pretreatment:

  Number or list of numbers 1:13 corresponding to desired pretreatment
  method(s):

  1.  Raw data (default)

  2.  Standard normal variate (SNV)

  3.  SNV and first derivative

  4.  SNV and second derivative

  5.  First derivative

  6.  Second derivative

  7.  Savitzky–Golay filter (SG)

  8.  SNV and SG

  9.  Gap-segment derivative (window size = 11)

  10. SG and first derivative (window size = 5)

  11. SG and first derivative (window size = 11)

  12. SG and second derivative (window size = 5)

  13. SG and second derivative (window size = 11)

- model.save.folder:

  Path to folder where model will be saved. If not provided, will save
  to working directory.

- model.name:

  Name that model will be saved as in `model.save.folder`. Default is
  "PredictionModel".

- best.model.metric:

  Metric used to decide which model is best. Must be either "RMSE" or
  "Rsquared"

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

- num.iterations:

  Number of training iterations to perform

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

- wavelengths:

  DEPRECATED `wavelengths` is no longer supported; this information is
  now inferred from `df` column names

- autoselect.preprocessing:

  DEPRECATED `autoselect.preprocessing = FALSE` is no longer supported.
  If multiple pretreatment methods are supplied, the best will be
  automatically selected as the model to be saved.

- preprocessing.method:

  DEPRECATED `preprocessing.method` has been renamed "pretreatment"

## Value

List of model stats (in `data.frame`) and trained model object. If the
parameter `write.model` is TRUE, both objects are saved to
`model.save.folder`. To use the optimally trained model for predictions,
use tuned parameters from `$bestTune`.

## Details

Wrapper that uses
[`pretreat_spectra`](https://GoreLab.github.io/waves/dev/reference/pretreat_spectra.md),
[`format_cv`](https://GoreLab.github.io/waves/dev/reference/format_cv.md),
and
[`train_spectra`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
functions.

## Author

Jenna Hershberger <jmh579@cornell.edu>

## Examples

``` r
# \donttest{
library(magrittr)
test.model <- ikeogu.2017 %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::rename(reference = DMC.oven,
                unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  save_model(
    df = .,
    write.model = FALSE,
    pretreatment = 1:13,
    model.name = "my_prediction_model",
    tune.length = 3,
    num.iterations = 3
  )
#> Warning: The `save.model` argument of `test_spectra()` is deprecated as of waves 0.2.0.
#> ℹ Models are now saved by default.
#> ℹ The deprecated feature was likely used in the waves package.
#>   Please report the issue at <https://github.com/GoreLab/waves/issues>.
#> Warning: The `return.model` argument of `test_spectra()` is deprecated as of waves
#> 0.2.0.
#> ℹ Trained models are now returned by default.
#> ℹ The deprecated feature was likely used in the waves package.
#>   Please report the issue at <https://github.com/GoreLab/waves/issues>.
#> Pretreatment initiated.
#> Training models...
#> Working on Raw_data 
#> Warning: The `wavelengths` argument of `train_spectra()` is deprecated as of waves
#> 0.2.0.
#> ℹ Wavelength specification is now inferred from column names.
#> ℹ The deprecated feature was likely used in the waves package.
#>   Please report the issue at <https://github.com/GoreLab/waves/issues>.
#> Warning: The `preprocessing` argument of `train_spectra()` is deprecated as of waves
#> 0.2.0.
#> ℹ Argument `preprocessing` is deprecated. Use `pretreatment` instead:
#>   `pretreatment = 1:13` (all), or `pretreatment = 1` (raw only).
#> ℹ The deprecated feature was likely used in the waves package.
#>   Please report the issue at <https://github.com/GoreLab/waves/issues>.
#> Warning: The `save.model` argument of `train_spectra()` is deprecated as of waves 0.2.0.
#> ℹ Models are now saved by default.
#> ℹ The deprecated feature was likely used in the waves package.
#>   Please report the issue at <https://github.com/GoreLab/waves/issues>.
#> Loading required package: lattice
#> 
#> Attaching package: ‘pls’
#> The following object is masked from ‘package:caret’:
#> 
#>     R2
#> The following object is masked from ‘package:stats’:
#> 
#>     loadings
#> Returning model...
#> Working on SNV 
#> Returning model...
#> Working on SNV1D 
#> Returning model...
#> Working on SNV2D 
#> Returning model...
#> Working on D1 
#> Returning model...
#> Working on D2 
#> Returning model...
#> Working on SG 
#> Returning model...
#> Working on SNVSG 
#> Returning model...
#> Working on SGD1 
#> Returning model...
#> Working on SG.D1W5 
#> Returning model...
#> Working on SG.D1W11 
#> Returning model...
#> Working on SG.D2W5 
#> Returning model...
#> Working on SG.D2W11 
#> Returning model...
#> 
#> Training Summary:
#> # A tibble: 13 × 40
#>    Pretreatment RMSEp_mean R2p_mean RPD_mean RPIQ_mean CCC_mean Bias_mean
#>    <chr>             <dbl>    <dbl>    <dbl>     <dbl>    <dbl>     <dbl>
#>  1 Raw_data           2.17   0.774     2.16       2.50    0.873    0.141 
#>  2 SNV                1.73   0.860     2.64       3.28    0.916   -0.0808
#>  3 SNV1D              2.61   0.704     1.74       2.18    0.781   -0.119 
#>  4 SNV2D              4.56   0.0323    0.992      1.25    0.118    0.420 
#>  5 D1                 2.46   0.720     1.86       2.31    0.799   -0.0538
#>  6 D2                 4.56   0.0329    0.992      1.25    0.117    0.449 
#>  7 SG                 2.06   0.817     2.20       2.74    0.886   -0.245 
#>  8 SNVSG              1.70   0.863     2.69       3.33    0.918   -0.0813
#>  9 SGD1               2.02   0.801     2.27       2.81    0.880   -0.158 
#> 10 SG.D1W5            2.05   0.798     2.25       2.79    0.869   -0.149 
#> 11 SG.D1W11           1.97   0.809     2.33       2.89    0.884   -0.165 
#> 12 SG.D2W5            4.32   0.0950    1.05       1.32    0.213    0.199 
#> 13 SG.D2W11           2.70   0.665     1.70       2.11    0.759    0.0578
#> # ℹ 33 more variables: SEP_mean <dbl>, RMSEcv_mean <dbl>, R2cv_mean <dbl>,
#> #   R2sp_mean <dbl>, best.ncomp_mean <dbl>, best.ntree_mean <dbl>,
#> #   best.mtry_mean <dbl>, RMSEp_sd <dbl>, R2p_sd <dbl>, RPD_sd <dbl>,
#> #   RPIQ_sd <dbl>, CCC_sd <dbl>, Bias_sd <dbl>, SEP_sd <dbl>, RMSEcv_sd <dbl>,
#> #   R2cv_sd <dbl>, R2sp_sd <dbl>, best.ncomp_sd <dbl>, best.ntree_sd <dbl>,
#> #   best.mtry_sd <dbl>, RMSEp_mode <dbl>, R2p_mode <dbl>, RPD_mode <dbl>,
#> #   RPIQ_mode <dbl>, CCC_mode <dbl>, Bias_mode <dbl>, SEP_mode <dbl>, …
#> 
#> Best pretreatment technique: SNVSG
summary(test.model$best.model)
#> Data:    X dimension: 120 2141 
#>  Y dimension: 120 1
#> Fit method: kernelpls
#> Number of components considered: 3
#> TRAINING: % variance explained
#>            1 comps  2 comps  3 comps
#> X            64.48    87.94    91.43
#> reference    33.93    64.97    87.18
test.model$best.model.stats
#> # A tibble: 1 × 40
#>   Pretreatment RMSEp_mean R2p_mean RPD_mean RPIQ_mean CCC_mean Bias_mean
#>   <chr>             <dbl>    <dbl>    <dbl>     <dbl>    <dbl>     <dbl>
#> 1 SNVSG              1.70    0.863     2.69      3.33    0.918   -0.0813
#> # ℹ 33 more variables: SEP_mean <dbl>, RMSEcv_mean <dbl>, R2cv_mean <dbl>,
#> #   R2sp_mean <dbl>, best.ncomp_mean <dbl>, best.ntree_mean <dbl>,
#> #   best.mtry_mean <dbl>, RMSEp_sd <dbl>, R2p_sd <dbl>, RPD_sd <dbl>,
#> #   RPIQ_sd <dbl>, CCC_sd <dbl>, Bias_sd <dbl>, SEP_sd <dbl>, RMSEcv_sd <dbl>,
#> #   R2cv_sd <dbl>, R2sp_sd <dbl>, best.ncomp_sd <dbl>, best.ntree_sd <dbl>,
#> #   best.mtry_sd <dbl>, RMSEp_mode <dbl>, R2p_mode <dbl>, RPD_mode <dbl>,
#> #   RPIQ_mode <dbl>, CCC_mode <dbl>, Bias_mode <dbl>, SEP_mode <dbl>, …
# }
```
