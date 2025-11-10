# Use provided model object to predict trait values with input dataset

Loads an existing model and cross-validation performance statistics
(created with
[`save_model`](https://GoreLab.github.io/waves/dev/reference/save_model.md))
and makes predictions based on new spectra.

## Usage

``` r
predict_spectra(
  input.data,
  model.stats.location,
  model.location,
  model.method = "pls",
  wavelengths = lifecycle::deprecated()
)
```

## Arguments

- input.data:

  `data.frame` object of spectral data for input into a spectral
  prediction model. First column contains unique identifiers followed by
  spectral columns. Include no other columns to right of spectra! Column
  names of spectra must start with "X".

- model.stats.location:

  String containing file path (including file name) to save location of
  "(model.name)\_stats.csv" as output from the
  [`save_model`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  function.

- model.location:

  String containing file path (including file name) to location where
  the trained model ("(model.name).Rds") was saved as output by the
  [`save_model`](https://GoreLab.github.io/waves/dev/reference/save_model.md)
  function.

- model.method:

  Model type to use for training. Valid options include:

  - "pls": Partial least squares regression (Default)

  - "rf": Random forest

  - "svmLinear": Support vector machine with linear kernel

  - "svmRadial": Support vector machine with radial kernel

- wavelengths:

  DEPRECATED `wavelengths` is no longer supported; this information is
  now inferred from `input.data` column names

## Value

`data.frame` object of predictions for each sample (row). First column
is unique identifier supplied by `input.data` and second is predicted
values

## Author

Jenna Hershberger <jmh579@cornell.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
ikeogu.2017 %>%
  dplyr::select(sample.id, dplyr::starts_with("X")) %>%
  predict_spectra(
    input.data = .,
    model.stats.location = paste0(
      getwd(),
      "/my_model_stats.csv"
    ),
    model.location = paste0(getwd(), "/my_model.Rds")
  )
} # }
```
