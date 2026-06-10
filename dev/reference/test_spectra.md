# Test the performance of spectral models

Wrapper that trains models based spectral data to predict reference
values and reports model performance statistics

## Usage

``` r
test_spectra(
  train.data,
  num.iterations,
  test.data = NULL,
  pretreatment = 1,
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
  wavelengths = lifecycle::deprecated(),
  preprocessing = lifecycle::deprecated(),
  output.summary = lifecycle::deprecated(),
  rf.variable.importance = lifecycle::deprecated()
)
```

## Arguments

- train.data:

  `data.frame` object of spectral data for input into a spectral
  prediction model. First column contains unique identifiers, second
  contains reference values, followed by spectral columns. Include no
  other columns to right of spectra! Column names of spectra must start
  with "X" and reference column must be named "reference".

- num.iterations:

  Number of training iterations to perform

- test.data:

  `data.frame` with same specifications as `df`. Use if specific test
  set is desired for hyperparameter tuning. If `NULL`, function will
  automatically train with a stratified sample of 70%. Default is
  `NULL`.

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

- wavelengths:

  DEPRECATED `wavelengths` is no longer supported; this information is
  now inferred from `df` column names

- preprocessing:

  DEPRECATED please use `pretreatment` to specify the specific
  pretreatment(s) to test. For behavior identical to that of
  `preprocessing = TRUE`, set `pretreatment = 1:13`\`.

- output.summary:

  DEPRECATED `output.summary = FALSE` is no longer supported; a summary
  of output is always returned alongside the full performance
  statistics.

- rf.variable.importance:

  DEPRECATED `rf.variable.importance = FALSE` is no longer supported;
  variable importance results are always returned if the `model.method`
  is set to \`pls\`, \`rf\`, or \`svmLinear\`.

## Value

`list` of 5 objects:

1.  \`model.list\` is a `list` of trained model objects, one for each
    pretreatment method specified by the `pretreatment` argument. Each
    model is trained with all rows of `df`.

2.  \`summary.model.performance\` is a `data.frame` containing summary
    statistics across all model training iterations and pretreatments.
    See below for a description of the summary statistics provided.

3.  \`model.performance\` is a `data.frame` containing performance
    statistics for each iteration of model training separately (see
    below).

4.  \`predictions\` is a `data.frame` containing both reference and
    predicted values for each test set entry in each iteration of model
    training.

5.  \`importance\` is a `data.frame` containing variable importance
    results for each wavelength at each iteration of model training.
    Variable importance is not available for
    `model.method = "svmRadial"` and this list item will be `NULL`. The
    importance measure differs by model type:

    - **PLS**: weighted sums of absolute regression coefficients,
      weighted by the reduction in sums of squares across components
      (via [`caret::varImp`](https://rdrr.io/pkg/caret/man/varImp.html))

    - **RF**: mean decrease in accuracy/MSE from permuting each variable
      across trees, normalized by standard error (via
      [`caret::varImp`](https://rdrr.io/pkg/caret/man/varImp.html))

    - **svmLinear**: absolute values of the primal weight vector
      (\\\|w\| = \|X\_{sv}^T \alpha\|\\), where \\X\_{sv}\\ are the
      support vectors and \\\alpha\\ are the dual coefficients

\`summary.model.performance\` and \`model.performance\` `data.frames`
summary statistics include:

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

## Details

Calls
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
ikeogu.2017 %>%
  dplyr::rename(reference = DMC.oven,
                unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit() %>%
  test_spectra(
    train.data = .,
    tune.length = 3,
    num.iterations = 3,
    pretreatment = 1
  )
#> Pretreatment initiated.
#> Training models...
#> Working on Raw_data 
#> Returning model...
#> $model
#> Partial least squares regression, fitted with the kernel algorithm.
#> Call:
#> pls::plsr(formula = reference ~ spectra, ncomp = get_mode(results.df$best.ncomp),     data = df.plsr)
#> 
#> $summary.model.performance
#>   SummaryType ModelType     RMSEp        R2p      RPD      RPIQ        CCC
#> 1        mean       pls 2.0440540 0.77824381 2.085018 2.6038009 0.85155095
#> 2          sd       pls 0.2661027 0.02093217 0.183391 0.3062501 0.04696515
#> 3        mode       pls 1.8971232 0.78270780 2.139263 2.7700624 0.86628031
#>          Bias       SEP     RMSEcv      R2cv       R2sp best.ncomp best.ntree
#> 1 -0.05516781 2.0652364 1.95463427 0.7755627 0.77670789          3         NA
#> 2  0.09468830 0.2688604 0.08412512 0.0157923 0.01211986          0         NA
#> 3 -0.13588174 1.9167831 1.96423744 0.7807426 0.76866372          3         NA
#>   best.mtry
#> 1        NA
#> 2        NA
#> 3        NA
#> 
#> $model.performance
#>   Iteration ModelType    RMSEp       R2p      RPD     RPIQ       CCC
#> 1         1       pls 1.897123 0.7827078 2.139263 2.770062 0.8662803
#> 2         2       pls 1.883812 0.7965839 2.235167 2.790961 0.8893859
#> 3         3       pls 2.351227 0.7554397 1.880623 2.250380 0.7989866
#>          Bias      SEP   RMSEcv      R2cv      R2sp best.ncomp best.ntree
#> 1 -0.13588174 1.916783 1.964237 0.7807426 0.7686637          3         NA
#> 2  0.04906262 1.903334 2.033546 0.7578310 0.7708123          3         NA
#> 3 -0.07868431 2.375593 1.866120 0.7881145 0.7906476          3         NA
#>   best.mtry
#> 1        NA
#> 2        NA
#> 3        NA
#> 
#> $predictions
#>     Iteration ModelType   unique.id reference predicted
#> 1           1       pls   C16Mcal_3  42.04462  39.73413
#> 2           1       pls  C16Mcal_11  35.23000  37.05843
#> 3           1       pls  C16Mcal_14  42.23797  41.23787
#> 4           1       pls  C16Mcal_16  36.37963  38.47196
#> 5           1       pls  C16Mcal_17  36.62819  38.38934
#> 6           1       pls  C16Mcal_21  37.61227  37.58763
#> 7           1       pls  C16Mcal_23  37.14000  36.16229
#> 8           1       pls  C16Mcal_24  42.19112  40.00201
#> 9           1       pls  C16Mcal_28  29.21000  31.80886
#> 10          1       pls  C16Mcal_34  42.53000  38.72081
#> 11          1       pls  C16Mcal_36  36.40311  36.24681
#> 12          1       pls  C16Mcal_37  36.74377  37.09421
#> 13          1       pls  C16Mcal_38  33.78840  35.81316
#> 14          1       pls  C16Mcal_40  36.13000  36.94475
#> 15          1       pls  C16Mcal_46  32.38298  33.34937
#> 16          1       pls  C16Mcal_50  37.71000  34.05821
#> 17          1       pls  C16Mcal_52  39.11203  37.99830
#> 18          1       pls  C16Mcal_53  41.46000  39.59285
#> 19          1       pls  C16Mcal_64  27.26000  28.69082
#> 20          1       pls  C16Mcal_68  33.17000  34.08023
#> 21          1       pls  C16Mcal_70  39.51004  37.68693
#> 22          1       pls  C16Mcal_77  33.85688  29.71884
#> 23          1       pls  C16Mcal_78  39.37000  37.66322
#> 24          1       pls  C16Mcal_79  35.99000  38.70026
#> 25          1       pls  C16Mcal_82  34.87000  30.40309
#> 26          1       pls  C16Mcal_83  34.15620  33.30617
#> 27          1       pls  C16Mcal_85  33.13301  32.40154
#> 28          1       pls  C16Mcal_89  40.60851  39.93190
#> 29          1       pls  C16Mcal_92  34.42487  34.77004
#> 30          1       pls C16Mcal_107  38.87000  37.13854
#> 31          1       pls C16Mcal_109  28.94000  31.36524
#> 32          1       pls C16Mcal_111  37.64000  36.89558
#> 33          1       pls C16Mcal_113  33.47000  35.53501
#> 34          1       pls C16Mcal_116  43.23313  41.74664
#> 35          1       pls   C16Mval_2  43.74113  41.36972
#> 36          1       pls   C16Mval_5  41.75497  42.04256
#> 37          1       pls  C16Mval_10  39.39553  37.62582
#> 38          1       pls  C16Mval_13  38.67529  37.25103
#> 39          1       pls  C16Mval_15  38.50998  39.07893
#> 40          1       pls  C16Mval_17  38.39624  38.27696
#> 41          1       pls  C16Mval_25  36.51727  36.46208
#> 42          1       pls  C16Mval_28  36.38000  36.83738
#> 43          1       pls  C16Mval_32  35.91000  37.18442
#> 44          1       pls  C16Mval_39  34.57000  37.11280
#> 45          1       pls  C16Mval_40  34.29912  34.33355
#> 46          1       pls  C16Mval_46  33.41928  34.16409
#> 47          1       pls  C16Mval_47  31.10258  34.46035
#> 48          1       pls  C16Mval_49  30.81136  33.09713
#> 49          1       pls  C16Mval_53  27.34904  28.00849
#> 50          2       pls   C16Mcal_4  39.00999  36.97214
#> 51          2       pls  C16Mcal_12  41.97913  40.81870
#> 52          2       pls  C16Mcal_19  39.70911  38.62620
#> 53          2       pls  C16Mcal_22  41.28000  40.57772
#> 54          2       pls  C16Mcal_23  37.14000  36.60694
#> 55          2       pls  C16Mcal_24  42.19112  39.77167
#> 56          2       pls  C16Mcal_25  31.76563  34.10483
#> 57          2       pls  C16Mcal_29  39.64507  40.50481
#> 58          2       pls  C16Mcal_33  34.97572  34.09109
#> 59          2       pls  C16Mcal_41  39.93124  39.23849
#> 60          2       pls  C16Mcal_42  34.72000  34.22190
#> 61          2       pls  C16Mcal_45  29.94000  31.66503
#> 62          2       pls  C16Mcal_49  38.28000  37.99754
#> 63          2       pls  C16Mcal_50  37.71000  34.51421
#> 64          2       pls  C16Mcal_53  41.46000  39.27199
#> 65          2       pls  C16Mcal_63  30.90000  31.73523
#> 66          2       pls  C16Mcal_67  36.26282  37.82109
#> 67          2       pls  C16Mcal_69  31.96079  30.15139
#> 68          2       pls  C16Mcal_71  40.02523  38.77683
#> 69          2       pls  C16Mcal_73  29.92186  31.28319
#> 70          2       pls  C16Mcal_74  32.09270  34.92711
#> 71          2       pls  C16Mcal_80  31.55000  33.87377
#> 72          2       pls  C16Mcal_81  37.66915  38.76739
#> 73          2       pls  C16Mcal_84  34.19000  33.67867
#> 74          2       pls  C16Mcal_87  35.05209  38.02618
#> 75          2       pls  C16Mcal_89  40.60851  39.63145
#> 76          2       pls  C16Mcal_93  34.82713  35.74532
#> 77          2       pls  C16Mcal_96  36.23665  39.15187
#> 78          2       pls C16Mcal_103  39.35234  39.22313
#> 79          2       pls C16Mcal_109  28.94000  31.62878
#> 80          2       pls C16Mcal_110  23.59213  19.27196
#> 81          2       pls C16Mcal_115  37.89000  34.08443
#> 82          2       pls C16Mcal_121  34.33449  35.42712
#> 83          2       pls   C16Mval_8  39.82226  37.75198
#> 84          2       pls  C16Mval_10  39.39553  37.54001
#> 85          2       pls  C16Mval_11  38.89882  38.06222
#> 86          2       pls  C16Mval_16  38.48635  38.21584
#> 87          2       pls  C16Mval_18  38.12000  39.75605
#> 88          2       pls  C16Mval_20  37.92153  37.63294
#> 89          2       pls  C16Mval_22  37.48000  35.88603
#> 90          2       pls  C16Mval_23  37.35000  37.85863
#> 91          2       pls  C16Mval_31  36.03212  35.03703
#> 92          2       pls  C16Mval_32  35.91000  37.69580
#> 93          2       pls  C16Mval_33  35.46458  33.94265
#> 94          2       pls  C16Mval_38  34.65316  37.31160
#> 95          2       pls  C16Mval_44  33.75234  35.33833
#> 96          2       pls  C16Mval_49  30.81136  33.05053
#> 97          2       pls  C16Mval_51  28.30972  32.02043
#> 98          2       pls  C16Mval_53  27.34904  28.01556
#> 99          3       pls   C16Mcal_9  38.12000  37.61363
#> 100         3       pls  C16Mcal_10  31.79933  33.99608
#> 101         3       pls  C16Mcal_12  41.97913  40.72232
#> 102         3       pls  C16Mcal_14  42.23797  40.03229
#> 103         3       pls  C16Mcal_24  42.19112  38.74104
#> 104         3       pls  C16Mcal_32  37.47000  37.80858
#> 105         3       pls  C16Mcal_34  42.53000  38.78957
#> 106         3       pls  C16Mcal_41  39.93124  39.27036
#> 107         3       pls  C16Mcal_44  43.29622  38.98826
#> 108         3       pls  C16Mcal_45  29.94000  33.02270
#> 109         3       pls  C16Mcal_54  35.05442  34.78421
#> 110         3       pls  C16Mcal_56  38.48000  37.65017
#> 111         3       pls  C16Mcal_59  28.98000  35.76920
#> 112         3       pls  C16Mcal_60  36.62512  37.10832
#> 113         3       pls  C16Mcal_64  27.26000  28.35505
#> 114         3       pls  C16Mcal_65  38.80697  37.03975
#> 115         3       pls  C16Mcal_72  40.12136  36.99847
#> 116         3       pls  C16Mcal_73  29.92186  31.51435
#> 117         3       pls  C16Mcal_76  35.25478  34.53688
#> 118         3       pls  C16Mcal_79  35.99000  39.10992
#> 119         3       pls  C16Mcal_80  31.55000  33.99310
#> 120         3       pls  C16Mcal_85  33.13301  33.53000
#> 121         3       pls  C16Mcal_88  34.66000  36.35874
#> 122         3       pls  C16Mcal_92  34.42487  35.45899
#> 123         3       pls  C16Mcal_93  34.82713  35.78262
#> 124         3       pls  C16Mcal_97  34.21594  35.29127
#> 125         3       pls C16Mcal_100  34.83110  36.19697
#> 126         3       pls C16Mcal_101  41.60015  39.33334
#> 127         3       pls C16Mcal_104  35.10946  34.36709
#> 128         3       pls C16Mcal_105  41.13854  37.47349
#> 129         3       pls C16Mcal_108  39.17000  38.22545
#> 130         3       pls C16Mcal_109  28.94000  32.33037
#> 131         3       pls C16Mcal_111  37.64000  36.61518
#> 132         3       pls C16Mcal_112  40.93710  38.87781
#> 133         3       pls C16Mcal_115  37.89000  36.00755
#> 134         3       pls C16Mcal_119  31.16000  29.01661
#> 135         3       pls   C16Mval_4  43.14000  40.36667
#> 136         3       pls  C16Mval_10  39.39553  37.11115
#> 137         3       pls  C16Mval_12  38.69113  38.99076
#> 138         3       pls  C16Mval_20  37.92153  37.60176
#> 139         3       pls  C16Mval_22  37.48000  35.71173
#> 140         3       pls  C16Mval_24  36.77462  36.83839
#> 141         3       pls  C16Mval_28  36.38000  36.74491
#> 142         3       pls  C16Mval_29  36.23446  36.26530
#> 143         3       pls  C16Mval_34  35.18792  32.79549
#> 144         3       pls  C16Mval_42  33.87885  35.69421
#> 145         3       pls  C16Mval_49  30.81136  33.40937
#> 146         3       pls  C16Mval_52  28.00003  34.26481
#> 147         3       pls  C16Mval_53  27.34904  28.10151
#> 
#> $importance
#> # A tibble: 3 × 2,153
#>   Iteration ModelType   X350   X351   X352   X353   X354   X355   X356   X357
#>       <int> <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1         1 pls       0.0258 0.0245 0.0278 0.0268 0.0265 0.0311 0.0343 0.0312
#> 2         2 pls       0.0351 0.0344 0.0329 0.0315 0.0328 0.0365 0.0416 0.0404
#> 3         3 pls       0.0241 0.0251 0.0256 0.0258 0.0253 0.0292 0.0324 0.0310
#> # ℹ 2,143 more variables: X358 <dbl>, X359 <dbl>, X360 <dbl>, X361 <dbl>,
#> #   X362 <dbl>, X363 <dbl>, X364 <dbl>, X365 <dbl>, X366 <dbl>, X367 <dbl>,
#> #   X368 <dbl>, X369 <dbl>, X370 <dbl>, X371 <dbl>, X372 <dbl>, X373 <dbl>,
#> #   X374 <dbl>, X375 <dbl>, X376 <dbl>, X377 <dbl>, X378 <dbl>, X379 <dbl>,
#> #   X380 <dbl>, X381 <dbl>, X382 <dbl>, X383 <dbl>, X384 <dbl>, X385 <dbl>,
#> #   X386 <dbl>, X387 <dbl>, X388 <dbl>, X389 <dbl>, X390 <dbl>, X391 <dbl>,
#> #   X392 <dbl>, X393 <dbl>, X394 <dbl>, X395 <dbl>, X396 <dbl>, X397 <dbl>, …
#> 
# }
```
