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
#> plsr(formula = reference ~ spectra, ncomp = tune.length, data = df.plsr)
#> 
#> $summary.model.performance
#>   SummaryType ModelType     RMSEp        R2p       RPD      RPIQ       CCC
#> 1        mean       pls 2.2768769 0.70454885 1.8508044 2.3625026 0.8283973
#> 2          sd       pls 0.2810942 0.05375604 0.1499152 0.3520937 0.0307392
#> 3        mode       pls 2.5989441 0.64247793 1.6777017 1.9660615 0.7942487
#>          Bias       SEP    RMSEcv       R2cv       R2sp best.ncomp best.ntree
#> 1  0.17002678 2.3004721 1.9949798 0.76826870 0.68697552  2.6666667         NA
#> 2  0.17115655 0.2840072 0.0472343 0.01823408 0.08861448  0.5773503         NA
#> 3 -0.01066911 2.6258769 2.0495126 0.74746541 0.59936036  3.0000000         NA
#>   best.mtry
#> 1        NA
#> 2        NA
#> 3        NA
#> 
#> $model.performance
#>   Iteration ModelType    RMSEp       R2p      RPD     RPIQ       CCC
#> 1         1       pls 2.598944 0.6424779 1.677702 1.966062 0.7942487
#> 2         2       pls 2.150750 0.7359187 1.936286 2.482647 0.8370868
#> 3         3       pls 2.080936 0.7352500 1.938426 2.638799 0.8538563
#>          Bias      SEP   RMSEcv      R2cv      R2sp best.ncomp best.ntree
#> 1 -0.01066911 2.625877 2.049513 0.7474654 0.5993604          2         NA
#> 2  0.32970230 2.173038 1.968558 0.7758599 0.7765566          3         NA
#> 3  0.19104716 2.102501 1.966869 0.7814808 0.6850096          3         NA
#>   best.mtry
#> 1        NA
#> 2        NA
#> 3        NA
#> 
#> $predictions
#>     Iteration ModelType   unique.id reference predicted
#> 1           1       pls   C16Mcal_3  42.04462  38.54056
#> 2           1       pls   C16Mcal_8  26.27746  30.48956
#> 3           1       pls  C16Mcal_12  41.97913  39.48373
#> 4           1       pls  C16Mcal_16  36.37963  38.10292
#> 5           1       pls  C16Mcal_18  39.56322  39.49788
#> 6           1       pls  C16Mcal_20  34.13000  32.51100
#> 7           1       pls  C16Mcal_22  41.28000  40.95222
#> 8           1       pls  C16Mcal_23  37.14000  35.88274
#> 9           1       pls  C16Mcal_27  38.89808  37.95011
#> 10          1       pls  C16Mcal_30  30.93000  30.21813
#> 11          1       pls  C16Mcal_32  37.47000  40.47471
#> 12          1       pls  C16Mcal_34  42.53000  39.28476
#> 13          1       pls  C16Mcal_36  36.40311  35.06344
#> 14          1       pls  C16Mcal_37  36.74377  36.69305
#> 15          1       pls  C16Mcal_38  33.78840  35.08072
#> 16          1       pls  C16Mcal_49  38.28000  38.59552
#> 17          1       pls  C16Mcal_50  37.71000  33.95837
#> 18          1       pls  C16Mcal_54  35.05442  34.57999
#> 19          1       pls  C16Mcal_58  39.88335  38.90822
#> 20          1       pls  C16Mcal_59  28.98000  38.10106
#> 21          1       pls  C16Mcal_62  29.07386  26.29317
#> 22          1       pls  C16Mcal_68  33.17000  34.63274
#> 23          1       pls  C16Mcal_71  40.02523  39.39838
#> 24          1       pls  C16Mcal_72  40.12136  35.79498
#> 25          1       pls  C16Mcal_80  31.55000  33.32765
#> 26          1       pls  C16Mcal_81  37.66915  37.91760
#> 27          1       pls  C16Mcal_84  34.19000  34.54866
#> 28          1       pls  C16Mcal_87  35.05209  37.34951
#> 29          1       pls  C16Mcal_88  34.66000  36.64785
#> 30          1       pls  C16Mcal_93  34.82713  35.51264
#> 31          1       pls C16Mcal_102  31.11491  31.28685
#> 32          1       pls C16Mcal_105  41.13854  38.68069
#> 33          1       pls C16Mcal_106  40.36111  35.77896
#> 34          1       pls C16Mcal_110  23.59213  21.25109
#> 35          1       pls   C16Mval_4  43.14000  40.76942
#> 36          1       pls  C16Mval_10  39.39553  37.77141
#> 37          1       pls  C16Mval_12  38.69113  40.86468
#> 38          1       pls  C16Mval_14  38.58000  35.82640
#> 39          1       pls  C16Mval_15  38.50998  39.40142
#> 40          1       pls  C16Mval_16  38.48635  37.41270
#> 41          1       pls  C16Mval_18  38.12000  41.84432
#> 42          1       pls  C16Mval_28  36.38000  37.63092
#> 43          1       pls  C16Mval_33  35.46458  33.27425
#> 44          1       pls  C16Mval_34  35.18792  34.77491
#> 45          1       pls  C16Mval_40  34.29912  34.35199
#> 46          1       pls  C16Mval_44  33.75234  34.84877
#> 47          1       pls  C16Mval_46  33.41928  35.23922
#> 48          1       pls  C16Mval_49  30.81136  32.68556
#> 49          1       pls  C16Mval_52  28.00003  34.24019
#> 50          2       pls   C16Mcal_2  35.52017  33.57397
#> 51          2       pls   C16Mcal_3  42.04462  39.99296
#> 52          2       pls  C16Mcal_10  31.79933  33.99013
#> 53          2       pls  C16Mcal_12  41.97913  40.96830
#> 54          2       pls  C16Mcal_23  37.14000  36.89125
#> 55          2       pls  C16Mcal_26  28.53000  32.44115
#> 56          2       pls  C16Mcal_38  33.78840  36.08390
#> 57          2       pls  C16Mcal_41  39.93124  39.41173
#> 58          2       pls  C16Mcal_43  37.91032  39.47767
#> 59          2       pls  C16Mcal_46  32.38298  33.47293
#> 60          2       pls  C16Mcal_48  37.06249  37.33896
#> 61          2       pls  C16Mcal_61  39.40313  40.84270
#> 62          2       pls  C16Mcal_62  29.07386  26.11999
#> 63          2       pls  C16Mcal_64  27.26000  28.13412
#> 64          2       pls  C16Mcal_65  38.80697  37.49045
#> 65          2       pls  C16Mcal_66  41.76970  38.61981
#> 66          2       pls  C16Mcal_73  29.92186  31.59402
#> 67          2       pls  C16Mcal_74  32.09270  35.13137
#> 68          2       pls  C16Mcal_75  37.49795  40.22167
#> 69          2       pls  C16Mcal_79  35.99000  39.25985
#> 70          2       pls  C16Mcal_82  34.87000  30.08495
#> 71          2       pls  C16Mcal_84  34.19000  34.37767
#> 72          2       pls  C16Mcal_85  33.13301  32.61708
#> 73          2       pls  C16Mcal_91  37.96000  37.66899
#> 74          2       pls  C16Mcal_96  36.23665  39.44119
#> 75          2       pls  C16Mcal_97  34.21594  35.37660
#> 76          2       pls C16Mcal_103  39.35234  39.12577
#> 77          2       pls C16Mcal_104  35.10946  34.55320
#> 78          2       pls C16Mcal_107  38.87000  37.52700
#> 79          2       pls C16Mcal_108  39.17000  38.18365
#> 80          2       pls C16Mcal_114  34.29000  35.29551
#> 81          2       pls C16Mcal_117  40.30803  39.47031
#> 82          2       pls   C16Mval_2  43.74113  40.79589
#> 83          2       pls   C16Mval_3  43.36289  41.79314
#> 84          2       pls   C16Mval_7  40.43132  39.63628
#> 85          2       pls   C16Mval_8  39.82226  37.86255
#> 86          2       pls  C16Mval_10  39.39553  37.49115
#> 87          2       pls  C16Mval_11  38.89882  38.22768
#> 88          2       pls  C16Mval_19  37.95000  39.43619
#> 89          2       pls  C16Mval_20  37.92153  37.60475
#> 90          2       pls  C16Mval_24  36.77462  37.12847
#> 91          2       pls  C16Mval_28  36.38000  37.04057
#> 92          2       pls  C16Mval_32  35.91000  37.82673
#> 93          2       pls  C16Mval_35  35.15013  35.53065
#> 94          2       pls  C16Mval_40  34.29912  34.59688
#> 95          2       pls  C16Mval_43  33.83044  34.10041
#> 96          2       pls  C16Mval_50  29.99727  33.37312
#> 97          2       pls  C16Mval_51  28.30972  32.28392
#> 98          2       pls  C16Mval_52  28.00003  34.43332
#> 99          3       pls   C16Mcal_1  39.62109  37.91659
#> 100         3       pls   C16Mcal_7  35.81000  37.84817
#> 101         3       pls   C16Mcal_8  26.27746  29.92855
#> 102         3       pls  C16Mcal_25  31.76563  33.66298
#> 103         3       pls  C16Mcal_28  29.21000  31.56058
#> 104         3       pls  C16Mcal_32  37.47000  38.66070
#> 105         3       pls  C16Mcal_42  34.72000  33.75557
#> 106         3       pls  C16Mcal_43  37.91032  39.34607
#> 107         3       pls  C16Mcal_45  29.94000  32.49040
#> 108         3       pls  C16Mcal_49  38.28000  37.95834
#> 109         3       pls  C16Mcal_51  39.34805  37.76094
#> 110         3       pls  C16Mcal_54  35.05442  34.58942
#> 111         3       pls  C16Mcal_56  38.48000  38.23306
#> 112         3       pls  C16Mcal_57  40.57812  38.25403
#> 113         3       pls  C16Mcal_60  36.62512  37.10780
#> 114         3       pls  C16Mcal_62  29.07386  25.57430
#> 115         3       pls  C16Mcal_65  38.80697  37.10930
#> 116         3       pls  C16Mcal_77  33.85688  29.68911
#> 117         3       pls  C16Mcal_79  35.99000  39.39723
#> 118         3       pls  C16Mcal_82  34.87000  30.54506
#> 119         3       pls  C16Mcal_86  39.51000  41.79079
#> 120         3       pls  C16Mcal_89  40.60851  39.20488
#> 121         3       pls  C16Mcal_90  40.72711  40.10113
#> 122         3       pls  C16Mcal_96  36.23665  39.19276
#> 123         3       pls C16Mcal_103  39.35234  39.00270
#> 124         3       pls C16Mcal_112  40.93710  39.03184
#> 125         3       pls C16Mcal_116  43.23313  41.94818
#> 126         3       pls   C16Mval_1  44.12970  41.35933
#> 127         3       pls   C16Mval_5  41.75497  41.88348
#> 128         3       pls   C16Mval_6  41.49408  39.34260
#> 129         3       pls   C16Mval_8  39.82226  38.02610
#> 130         3       pls  C16Mval_15  38.50998  38.69961
#> 131         3       pls  C16Mval_18  38.12000  40.07237
#> 132         3       pls  C16Mval_19  37.95000  39.55714
#> 133         3       pls  C16Mval_22  37.48000  35.72186
#> 134         3       pls  C16Mval_23  37.35000  38.38731
#> 135         3       pls  C16Mval_27  36.46656  39.20948
#> 136         3       pls  C16Mval_30  36.19277  35.45783
#> 137         3       pls  C16Mval_33  35.46458  33.68892
#> 138         3       pls  C16Mval_35  35.15013  35.36321
#> 139         3       pls  C16Mval_39  34.57000  38.59672
#> 140         3       pls  C16Mval_41  34.17281  35.13932
#> 141         3       pls  C16Mval_43  33.83044  34.07810
#> 142         3       pls  C16Mval_44  33.75234  34.66040
#> 143         3       pls  C16Mval_45  33.44248  35.65443
#> 144         3       pls  C16Mval_46  33.41928  34.43494
#> 145         3       pls  C16Mval_47  31.10258  34.61967
#> 146         3       pls  C16Mval_49  30.81136  33.04002
#> 147         3       pls  C16Mval_53  27.34904  27.33615
#> 
#> $importance
#> # A tibble: 3 × 2,153
#>   Iteration ModelType   X350   X351   X352   X353   X354   X355   X356   X357
#>       <int> <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1         1 pls       0.0338 0.0353 0.0360 0.0343 0.0350 0.0384 0.0401 0.0395
#> 2         2 pls       0.0232 0.0268 0.0256 0.0256 0.0266 0.0302 0.0294 0.0286
#> 3         3 pls       0.0286 0.0288 0.0308 0.0293 0.0293 0.0338 0.0364 0.0351
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
