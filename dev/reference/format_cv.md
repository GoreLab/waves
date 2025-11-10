# Format multiple trials with or without overlapping genotypes into training and test sets according to user-provided cross validation scheme

Standalone function that is also used within
[`train_spectra`](https://GoreLab.github.io/waves/dev/reference/train_spectra.md)
to divide trials or studies into training and test sets based on overlap
in trial environments and genotype entries

## Usage

``` r
format_cv(
  trial1,
  trial2,
  trial3 = NULL,
  cv.scheme,
  stratified.sampling = TRUE,
  proportion.train = 0.7,
  seed = NULL,
  remove.genotype = FALSE
)
```

## Arguments

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

- cv.scheme:

  A cross validation (CV) scheme from Jarquín et al., 2017. Options for
  `cv.scheme` include:

  - "CV1": untested lines in tested environments

  - "CV2": tested lines in tested environments

  - "CV0": tested lines in untested environments

  - "CV00": untested lines in untested environments

- stratified.sampling:

  If `TRUE`, training and test sets will be selected using stratified
  random sampling. Default is `TRUE`.

- proportion.train:

  Fraction of samples to include in the training set. Default is 0.7.

- seed:

  Number used in the function
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
  randomization. If `NULL`, no seed is set. Default is `NULL`.

- remove.genotype:

  boolean that, if `TRUE`, removes the "genotype" column is removed from
  the output `data.frame`. Default is `FALSE`.

## Value

List of data.frames (\$train.set, \$test.set) compiled according to
user-provided cross validation scheme.

## Details

Use of a cross-validation scheme requires a column in the input
`data.frame` named "genotype" to ensure proper sorting of training and
test sets. Variables `trial1` and `trial2` are required, while `trial 3`
is optional.

## References

Jarquín, D., C. Lemes da Silva, R. C. Gaynor, J. Poland, A. Fritz, R.
Howard, S. Battenfield, and J. Crossa. 2017. Increasing genomic-enabled
prediction accuracy by modeling genotype × environment interactions in
Kansas wheat. Plant Genome 10(2):1-15.
\<doi:10.3835/plantgenome2016.12.0130\>

## Author

Jenna Hershberger <jmh579@cornell.edu>

## Examples

``` r
# Must have a column called "genotype", so we'll create a fake one for now
# We will use CV00, which does not require any overlap in genotypes
# In real scenarios, CV schemes that rely on genotypes should not be applied
# when genotypes are unknown, as in this case.
library(magrittr)
trials <- ikeogu.2017 %>%
  dplyr::mutate(genotype = 1:nrow(ikeogu.2017)) %>% # fake for this example
  dplyr::rename(reference = DMC.oven) %>%
  dplyr::select(
    study.name, sample.id, genotype, reference,
    tidyselect::starts_with("X")
  )
trial1 <- trials %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::select(-study.name)
trial2 <- trials %>%
  dplyr::filter(study.name == "C16Mval") %>%
  dplyr::select(-study.name)
cv.list <- format_cv(
  trial1 = trial1, trial2 = trial2, cv.scheme = "CV00",
  stratified.sampling = FALSE, remove.genotype = TRUE
)
cv.list$train.set[1:5, 1:5]
#> # A tibble: 5 × 5
#>   sample.id reference  X350  X351  X352
#>   <chr>         <dbl> <dbl> <dbl> <dbl>
#> 1 C16Mval_1      44.1 0.497 0.474 0.489
#> 2 C16Mval_2      43.7 0.546 0.556 0.557
#> 3 C16Mval_3      43.4 0.513 0.521 0.529
#> 4 C16Mval_4      43.1 0.565 0.570 0.583
#> 5 C16Mval_5      41.8 0.497 0.502 0.510
cv.list$test.set[1:5, 1:5]
#> # A tibble: 5 × 5
#>   sample.id  reference  X350  X351  X352
#>   <chr>          <dbl> <dbl> <dbl> <dbl>
#> 1 C16Mcal_11      35.2 0.520 0.518 0.522
#> 2 C16Mcal_13      35.4 0.491 0.496 0.497
#> 3 C16Mcal_15      31.6 0.652 0.649 0.650
#> 4 C16Mcal_16      36.4 0.517 0.514 0.530
#> 5 C16Mcal_18      39.6 0.536 0.505 0.504
```
