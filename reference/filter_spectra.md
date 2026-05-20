# Filter spectral data frame based on Mahalanobis distance

Determine Mahalanobis distances of observations (rows) within a given
`data.frame` with spectral data. Option to filter out observations based
on these distances.

## Usage

``` r
filter_spectra(df, filter, return.distances, num.col.before.spectra,
  window.size, verbose)
```

## Arguments

- df:

  `data.frame` object containing columns of spectra and rows of
  observations. Spectral columns must be labeled with an "X" and then
  the wavelength (example: "X740" = 740nm). Left-most column must be
  unique ID. May also contain columns of metadata between the unique ID
  and spectral columns. Cannot contain any missing values. Metadata
  column names may not start with "X".

- filter:

  boolean that determines whether or not the input `data.frame` will be
  filtered. If `TRUE`, `df` will be filtered according to squared
  Mahalanobis distance with a 95% cutoff from a chi-square distribution
  with degrees of freedom = number of spectral columns. If `FALSE`, a
  column of squared Mahalanobis distances `h.distance` will be added to
  the right side of df and all rows will be returned. Default is `TRUE`.

- return.distances:

  boolean that determines whether a column of squared Mahalanobis
  distances will be included in output `data.frame`. If `TRUE`, a column
  of Mahalanobis distances for each row will be added to the right side
  of `df`. Default is `FALSE`.

- num.col.before.spectra:

  number of columns to the left of the spectral matrix in `df`. Default
  is 4.

- window.size:

  number defining the size of window to use when calculating the
  covariance of the spectra (required to calculate Mahalanobis
  distance). Default is 10.

- verbose:

  If `TRUE`, the number of rows removed through filtering will be
  printed to the console. Default is `TRUE`.

## Value

If `filter` is `TRUE`, returns filtered data frame `df` and reports the
number of rows removed. The Mahalanobis distance with a cutoff of 95% of
chi-square distribution (degrees of freedom = number of wavelengths) is
used as filtering criteria. If `filter` is `FALSE`, returns full input
df with column `h.distances` containing the Mahalanobis distance for
each row.

## Details

This function uses a chi-square distribution with 95% cutoff where
degrees of freedom = number of wavelengths (columns) in the input
`data.frame`.

## References

Johnson, R.A., and D.W. Wichern. 2007. Applied Multivariate Statistical
Analysis (6th Edition). pg 189

## Author

Jenna Hershberger <jmh579@cornell.edu>

## Examples

``` r
library(magrittr)
filtered.test <- ikeogu.2017 %>%
  dplyr::select(-TCC) %>%
  na.omit() %>%
  filter_spectra(
    df = .,
    filter = TRUE,
    return.distances = TRUE,
    num.col.before.spectra = 5,
    window.size = 15
  )
#> 
#> Removed 0 rows.
filtered.test[1:5, c(1:5, (ncol(filtered.test) - 5):ncol(filtered.test))]
#>   study.name sample.id DMC.oven      X350      X351    X2495    X2496    X2497
#> 1    C16Mcal C16Mcal_1 39.62109 0.4881079 0.4951843 1.867221 1.868554 1.866739
#> 2    C16Mcal C16Mcal_2 35.52017 0.5727389 0.5682541 1.886785 1.891462 1.893840
#> 3    C16Mcal C16Mcal_3 42.04462 0.5989934 0.6266454 1.838917 1.832336 1.834644
#> 4    C16Mcal C16Mcal_4 39.00999 0.5169374 0.5164186 1.841245 1.839129 1.837023
#> 5    C16Mcal C16Mcal_5 33.44273 0.5189608 0.5477946 1.891752 1.899751 1.900873
#>      X2498    X2499    X2500
#> 1 1.867465 1.870405 1.870702
#> 2 1.901451 1.891114 1.888507
#> 3 1.828793 1.826562 1.832022
#> 4 1.836635 1.835856 1.834857
#> 5 1.897076 1.899430 1.896130
```
