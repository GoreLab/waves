# Pretreat spectral data according to user-designated method

Pretreatment, also known as preprocessing, is often used to increase the
signal to noise ratio in vis-NIR datasets. The *waves* function
`pretreat_spectra` applies common spectral pretreatment methods such as
standard normal variate and the Savitzky-Golay filter.

## Usage

``` r
pretreat_spectra(
  df,
  test.data = NULL,
  pretreatment = 1,
  preprocessing.method = lifecycle::deprecated(),
  wavelengths = lifecycle::deprecated()
)
```

## Arguments

- df:

  `data.frame` object containing spectral data. First column(s)
  (optional) include metadata (with or without reference value column)
  followed by spectral columns. Spectral column names must be formatted
  as "X" followed by wavelength Include no other columns to right of
  spectra! No missing values permitted.

- test.data:

  `data.frame` object with same format as train.data. Will be appended
  to `df` during pretreatment so that the same transformations are
  applied to each row. Default is `NULL`.

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

- preprocessing.method:

  DEPRECATED `preprocessing.method` has been renamed "pretreatment"

- wavelengths:

  DEPRECATED `wavelengths` is no longer supported; this information is
  now inferred from `df` column names

## Value

Pretreated `df`\` (or list of `data.frame`s) with reference column
intact

## Author

Jenna Hershberger <jmh579@cornell.edu>

## Examples

``` r
pretreat_spectra(df = ikeogu.2017, pretreatment = 3)[1:5, 1:5]
#>   study.name sample.id DMC.oven       TCC         X351
#> 1    C16Mcal C16Mcal_1 39.62109  1.002029  0.013447658
#> 2    C16Mcal C16Mcal_2 35.52017 17.034718 -0.008428511
#> 3    C16Mcal C16Mcal_3 42.04462 21.616243  0.055000011
#> 4    C16Mcal C16Mcal_4 39.00999  2.428640 -0.001007054
#> 5    C16Mcal C16Mcal_5 33.44273 24.012182  0.054048209
```
