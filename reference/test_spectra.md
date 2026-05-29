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
  is set to \`pls\` or \`rf\`.

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
    results for each wavelength at each iteration of model training. If
    `model.method` is not "pls" or "rf", this list item is `NULL`.

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
[`pretreat_spectra`](https://GoreLab.github.io/waves/reference/pretreat_spectra.md),
[`format_cv`](https://GoreLab.github.io/waves/reference/format_cv.md),
and
[`train_spectra`](https://GoreLab.github.io/waves/reference/train_spectra.md)
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
#>   SummaryType ModelType     RMSEp        R2p       RPD      RPIQ        CCC
#> 1        mean       pls 1.9550337 0.79397360 2.2214284 2.8386768 0.88086475
#> 2          sd       pls 0.2489473 0.03040981 0.1618829 0.3850833 0.01865092
#> 3        mode       pls 2.1731908 0.76545308 2.0814944 2.5072139 0.86792697
#>         Bias       SEP     RMSEcv       R2cv       R2sp best.ncomp best.ntree
#> 1 -0.0138514 1.9752937 1.95600879 0.77110995 0.83091636          3         NA
#> 2  0.1929671 0.2515272 0.05736842 0.02161451 0.01186654          0         NA
#> 3  0.1393866 2.1957115 1.95054997 0.76339708 0.83312188          3         NA
#>   best.mtry
#> 1        NA
#> 2        NA
#> 3        NA
#> 
#> $model.performance
#>   Iteration ModelType    RMSEp       R2p      RPD     RPIQ       CCC
#> 1         1       pls 2.173191 0.7654531 2.081494 2.507214 0.8679270
#> 2         2       pls 2.008067 0.7904943 2.184064 2.747721 0.8724233
#> 3         3       pls 1.683843 0.8259734 2.398726 3.261096 0.9022440
#>          Bias      SEP   RMSEcv      R2cv      R2sp best.ncomp best.ntree
#> 1  0.13938660 2.195712 1.950550 0.7633971 0.8331219          3         NA
#> 2  0.04961844 2.028877 2.015912 0.7544099 0.8181018          3         NA
#> 3 -0.23055925 1.701293 1.901565 0.7955229 0.8415254          3         NA
#>   best.mtry
#> 1        NA
#> 2        NA
#> 3        NA
#> 
#> $predictions
#>     Iteration ModelType   unique.id reference predicted
#> 1           1       pls   C16Mcal_4  39.00999  36.50554
#> 2           1       pls   C16Mcal_8  26.27746  29.99556
#> 3           1       pls  C16Mcal_10  31.79933  33.91478
#> 4           1       pls  C16Mcal_11  35.23000  37.05689
#> 5           1       pls  C16Mcal_14  42.23797  40.37191
#> 6           1       pls  C16Mcal_16  36.37963  38.46198
#> 7           1       pls  C16Mcal_20  34.13000  32.12386
#> 8           1       pls  C16Mcal_33  34.97572  33.21051
#> 9           1       pls  C16Mcal_34  42.53000  38.92911
#> 10          1       pls  C16Mcal_36  36.40311  36.79291
#> 11          1       pls  C16Mcal_37  36.74377  37.07810
#> 12          1       pls  C16Mcal_38  33.78840  36.16584
#> 13          1       pls  C16Mcal_41  39.93124  39.59973
#> 14          1       pls  C16Mcal_45  29.94000  32.67283
#> 15          1       pls  C16Mcal_46  32.38298  33.66270
#> 16          1       pls  C16Mcal_53  41.46000  38.88892
#> 17          1       pls  C16Mcal_56  38.48000  37.90052
#> 18          1       pls  C16Mcal_59  28.98000  35.91302
#> 19          1       pls  C16Mcal_61  39.40313  40.58068
#> 20          1       pls  C16Mcal_69  31.96079  30.04438
#> 21          1       pls  C16Mcal_75  37.49795  40.17785
#> 22          1       pls  C16Mcal_85  33.13301  33.05767
#> 23          1       pls  C16Mcal_88  34.66000  36.29711
#> 24          1       pls  C16Mcal_90  40.72711  40.36644
#> 25          1       pls  C16Mcal_91  37.96000  38.27436
#> 26          1       pls  C16Mcal_94  39.54911  38.89206
#> 27          1       pls  C16Mcal_95  36.73743  36.18515
#> 28          1       pls  C16Mcal_97  34.21594  35.33449
#> 29          1       pls  C16Mcal_99  39.23705  37.33942
#> 30          1       pls C16Mcal_100  34.83110  36.24078
#> 31          1       pls C16Mcal_101  41.60015  39.44391
#> 32          1       pls C16Mcal_107  38.87000  37.73557
#> 33          1       pls C16Mcal_110  23.59213  19.28729
#> 34          1       pls C16Mcal_112  40.93710  39.24807
#> 35          1       pls C16Mcal_116  43.23313  41.59043
#> 36          1       pls C16Mcal_119  31.16000  28.70424
#> 37          1       pls   C16Mval_1  44.12970  40.80312
#> 38          1       pls   C16Mval_5  41.75497  41.58652
#> 39          1       pls  C16Mval_13  38.67529  38.22389
#> 40          1       pls  C16Mval_20  37.92153  37.76845
#> 41          1       pls  C16Mval_23  37.35000  38.46872
#> 42          1       pls  C16Mval_25  36.51727  37.22193
#> 43          1       pls  C16Mval_27  36.46656  39.21296
#> 44          1       pls  C16Mval_36  35.00251  33.78686
#> 45          1       pls  C16Mval_37  34.98731  36.38393
#> 46          1       pls  C16Mval_40  34.29912  34.51214
#> 47          1       pls  C16Mval_48  30.83729  32.64455
#> 48          1       pls  C16Mval_49  30.81136  33.26690
#> 49          1       pls  C16Mval_51  28.30972  31.95275
#> 50          2       pls   C16Mcal_7  35.81000  37.64045
#> 51          2       pls  C16Mcal_10  31.79933  33.90104
#> 52          2       pls  C16Mcal_13  35.35505  34.73994
#> 53          2       pls  C16Mcal_14  42.23797  40.32753
#> 54          2       pls  C16Mcal_15  31.65000  32.08634
#> 55          2       pls  C16Mcal_29  39.64507  40.03288
#> 56          2       pls  C16Mcal_31  32.40875  33.44434
#> 57          2       pls  C16Mcal_32  37.47000  37.95935
#> 58          2       pls  C16Mcal_34  42.53000  38.98974
#> 59          2       pls  C16Mcal_37  36.74377  36.91768
#> 60          2       pls  C16Mcal_40  36.13000  37.22844
#> 61          2       pls  C16Mcal_44  43.29622  39.26540
#> 62          2       pls  C16Mcal_46  32.38298  33.62651
#> 63          2       pls  C16Mcal_48  37.06249  37.36784
#> 64          2       pls  C16Mcal_51  39.34805  37.24778
#> 65          2       pls  C16Mcal_55  28.15000  31.67880
#> 66          2       pls  C16Mcal_57  40.57812  38.34005
#> 67          2       pls  C16Mcal_58  39.88335  39.56305
#> 68          2       pls  C16Mcal_60  36.62512  37.10264
#> 69          2       pls  C16Mcal_63  30.90000  31.96273
#> 70          2       pls  C16Mcal_64  27.26000  28.08015
#> 71          2       pls  C16Mcal_68  33.17000  33.76058
#> 72          2       pls  C16Mcal_76  35.25478  34.50975
#> 73          2       pls  C16Mcal_78  39.37000  38.77967
#> 74          2       pls  C16Mcal_83  34.15620  33.52379
#> 75          2       pls  C16Mcal_88  34.66000  36.31884
#> 76          2       pls  C16Mcal_92  34.42487  35.52188
#> 77          2       pls  C16Mcal_93  34.82713  35.99170
#> 78          2       pls  C16Mcal_94  39.54911  38.81317
#> 79          2       pls  C16Mcal_97  34.21594  35.24046
#> 80          2       pls  C16Mcal_98  38.83000  40.01306
#> 81          2       pls  C16Mcal_99  39.23705  37.28656
#> 82          2       pls C16Mcal_105  41.13854  37.65220
#> 83          2       pls C16Mcal_106  40.36111  37.23188
#> 84          2       pls C16Mcal_110  23.59213  19.76384
#> 85          2       pls C16Mcal_111  37.64000  36.53743
#> 86          2       pls C16Mcal_117  40.30803  39.02915
#> 87          2       pls C16Mcal_121  34.33449  35.50744
#> 88          2       pls   C16Mval_2  43.74113  40.57742
#> 89          2       pls  C16Mval_11  38.89882  38.28787
#> 90          2       pls  C16Mval_12  38.69113  39.23043
#> 91          2       pls  C16Mval_15  38.50998  38.39315
#> 92          2       pls  C16Mval_19  37.95000  39.26696
#> 93          2       pls  C16Mval_30  36.19277  35.52484
#> 94          2       pls  C16Mval_39  34.57000  38.15507
#> 95          2       pls  C16Mval_43  33.83044  34.24600
#> 96          2       pls  C16Mval_48  30.83729  32.68528
#> 97          2       pls  C16Mval_49  30.81136  33.22291
#> 98          2       pls  C16Mval_52  28.00003  34.22590
#> 99          3       pls   C16Mcal_5  33.44273  30.38350
#> 100         3       pls   C16Mcal_8  26.27746  30.38023
#> 101         3       pls   C16Mcal_9  38.12000  37.95717
#> 102         3       pls  C16Mcal_14  42.23797  41.11843
#> 103         3       pls  C16Mcal_17  36.62819  38.98191
#> 104         3       pls  C16Mcal_18  39.56322  38.04276
#> 105         3       pls  C16Mcal_20  34.13000  32.05256
#> 106         3       pls  C16Mcal_21  37.61227  37.43845
#> 107         3       pls  C16Mcal_25  31.76563  34.80034
#> 108         3       pls  C16Mcal_27  38.89808  39.37409
#> 109         3       pls  C16Mcal_29  39.64507  40.91366
#> 110         3       pls  C16Mcal_30  30.93000  30.03865
#> 111         3       pls  C16Mcal_31  32.40875  34.15086
#> 112         3       pls  C16Mcal_37  36.74377  37.17385
#> 113         3       pls  C16Mcal_42  34.72000  33.47320
#> 114         3       pls  C16Mcal_46  32.38298  33.99514
#> 115         3       pls  C16Mcal_47  31.60291  31.57118
#> 116         3       pls  C16Mcal_51  39.34805  39.09235
#> 117         3       pls  C16Mcal_52  39.11203  38.16011
#> 118         3       pls  C16Mcal_53  41.46000  40.62724
#> 119         3       pls  C16Mcal_57  40.57812  39.36704
#> 120         3       pls  C16Mcal_66  41.76970  39.96488
#> 121         3       pls  C16Mcal_68  33.17000  33.44908
#> 122         3       pls  C16Mcal_72  40.12136  36.76306
#> 123         3       pls  C16Mcal_76  35.25478  34.60691
#> 124         3       pls  C16Mcal_77  33.85688  29.56876
#> 125         3       pls  C16Mcal_84  34.19000  34.66959
#> 126         3       pls  C16Mcal_93  34.82713  34.37493
#> 127         3       pls C16Mcal_100  34.83110  36.64348
#> 128         3       pls C16Mcal_102  31.11491  30.62915
#> 129         3       pls C16Mcal_109  28.94000  31.70745
#> 130         3       pls   C16Mval_1  44.12970  41.91103
#> 131         3       pls   C16Mval_4  43.14000  41.17030
#> 132         3       pls   C16Mval_5  41.75497  42.21544
#> 133         3       pls   C16Mval_7  40.43132  39.24414
#> 134         3       pls   C16Mval_8  39.82226  39.14380
#> 135         3       pls  C16Mval_13  38.67529  37.33712
#> 136         3       pls  C16Mval_14  38.58000  37.64370
#> 137         3       pls  C16Mval_19  37.95000  38.85701
#> 138         3       pls  C16Mval_21  37.84025  38.13415
#> 139         3       pls  C16Mval_22  37.48000  36.04261
#> 140         3       pls  C16Mval_27  36.46656  38.45809
#> 141         3       pls  C16Mval_31  36.03212  35.34500
#> 142         3       pls  C16Mval_34  35.18792  34.04633
#> 143         3       pls  C16Mval_36  35.00251  33.43003
#> 144         3       pls  C16Mval_38  34.65316  37.29401
#> 145         3       pls  C16Mval_40  34.29912  33.93664
#> 146         3       pls  C16Mval_46  33.41928  33.38790
#> 147         3       pls  C16Mval_53  27.34904  27.53191
#> 
#> $importance
#> # A tibble: 3 × 2,153
#>   Iteration ModelType   X350   X351   X352   X353   X354   X355   X356   X357
#>       <int> <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1         1 pls       0.0246 0.0241 0.0260 0.0240 0.0252 0.0282 0.0310 0.0286
#> 2         2 pls       0.0248 0.0256 0.0270 0.0270 0.0263 0.0297 0.0311 0.0309
#> 3         3 pls       0.0407 0.0381 0.0374 0.0376 0.0376 0.0388 0.0439 0.0440
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
