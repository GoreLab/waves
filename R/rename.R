#' Functions renamed in waves 0.2.0
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' waves 0.2.0 renamed a number of functions to ensure that every function
#' name adheres to the tidyverse style guide.
#'
#' * `AggregateSpectra()` -> `aggregate_spectra()`
#' * `DoPreprocessing()` -> `pretreat_spectra()`
#' * `FilterSpectra()` -> `filter_spectra()`
#' * `FormatCV()` -> `format_cv()`
#' * `PlotSpectra()` -> `plot_spectra()`
#' * `PredictFromSavedModel()` -> `predict_spectra()`
#' * `SaveModel()` -> `save_model()`
#' * `TestModelPerformance()` -> `test_spectra()`
#' * `TrainSpectralModel()` -> `train_spectra()`
#'
#'
#' @keywords internal
#' @name rename
#' @aliases NULL
NULL

#' @rdname rename
#' @export
AggregateSpectra <- function(
    df,
    grouping.colnames = c("unique.id"),
    reference.value.colname = "reference",
    agg.function = "mean") {
  lifecycle::deprecate_warn("0.2.0", "AggregateSpectra()",
                            "aggregate_spectra()")
  aggregate_spectra(
    df,
    grouping.colnames = c("unique.id"),
    reference.value.colname = "reference",
    agg.function = "mean"
  )
}

#' @export
#' @rdname rename
DoPreprocessing <- function(
    df,
    test.data = NULL,
    pretreatment = 1) {
  lifecycle::deprecate_warn("0.2.0", "DoPreprocessing()", "pretreat_spectra()")
  pretreat_spectra(
    df,
    test.data = NULL,
    pretreatment = 1)
}

#' @rdname rename
#' @export
FilterSpectra <- function(
    df,
    filter = TRUE,
    return.distances = FALSE,
    num.col.before.spectra = 4,
    window.size = 10,
    verbose = TRUE) {
  lifecycle::deprecate_warn("0.2.0", "FilterSpectra()", "filter_spectra()")
  filter_spectra(
    df,
    filter = TRUE,
    return.distances = FALSE,
    num.col.before.spectra = 4,
    window.size = 10,
    verbose = TRUE
  )
}

#' @export
#' @rdname rename
FormatCV <- function(
    trial1,
    trial2,
    trial3 = NULL,
    cv.scheme,
    stratified.sampling = TRUE,
    proportion.train = 0.7,
    seed = NULL,
    remove.genotype = FALSE) {
  lifecycle::deprecate_warn("0.2.0", "FormatCV()", "format_cv()")
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
}

#' @export
#' @rdname rename
PlotSpectra <- function(
    df,
    num.col.before.spectra = 1,
    window.size = 10,
    detect.outliers = TRUE,
    color = NULL,
    alternate.title = NULL,
    verbose = TRUE) {
  lifecycle::deprecate_warn("0.2.0", "PlotSpectra()", "plot_spectra()")
  plot_spectra(
    df,
    num.col.before.spectra = 1,
    window.size = 10,
    detect.outliers = TRUE,
    color = NULL,
    alternate.title = NULL,
    verbose = TRUE
  )
}

#' @export
#' @rdname rename
PredictFromSavedModel <- function(
    input.data,
    model.stats.location,
    model.location,
    model.method = "pls") {
  lifecycle::deprecate_warn("0.2.0", "PredictFromSavedModel()",
                            "predict_spectra()")
  predict_spectra(
    input.data,
    model.stats.location,
    model.location,
    model.method = "pls"
  )
}

#' @export
#' @rdname rename
SaveModel <- function(
    df,
    save.model = TRUE,
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
    verbose = TRUE) {
  lifecycle::deprecate_warn("0.2.0", "SaveModel()", "save_model()")
  save_model(df,
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
    verbose = TRUE
  )
}

#' @export
#' @rdname rename
TestModelPerformance <- function(
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
    verbose = TRUE) {

  lifecycle::deprecate_warn("0.2.0", "TestModelPerformance()", "test_spectra()")
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
    verbose = TRUE
  )
}

#' @export
#' @rdname rename
TrainSpectralModel <- function(
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
    verbose = TRUE) {
  lifecycle::deprecate_warn("0.2.0", "TrainSpectralModel()", "train_spectra()")
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
    verbose = TRUE
  )
}
