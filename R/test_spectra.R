#' @title Test the performance of spectral models
#' @name test_spectra
#' @description Wrapper that trains models based spectral data to predict
#'   reference values and reports model performance statistics
#'
#' @details Calls \code{\link{pretreat_spectra}}, \code{\link{format_cv}},
#' and \code{\link{train_spectra}} functions.
#'
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams train_spectra
#' @inheritParams format_cv
#' @inheritParams pretreat_spectra
#' @param train.data \code{data.frame} object of spectral data for input into a
#'   spectral prediction model. First column contains unique identifiers, second
#'   contains reference values, followed by spectral columns. Include no other
#'   columns to right of spectra! Column names of spectra must start with "X"
#'   and reference column must be named "reference".
#' @param preprocessing DEPRECATED please use
#'   \code{pretreatment} to specify the specific pretreatment(s) to test.
#'   For behavior identical to that of \code{preprocessing = TRUE}, set
#'   \code{pretreatment = 1:13}`.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang abort
#' @importFrom lifecycle deprecated
#'
#' @return \code{list} of 5 objects:
#'   \enumerate{
#'   \item `model.list` is a \code{list} of trained model objects, one for each
#'   pretreatment method specified by the \code{pretreatment} argument.
#'   Each model is trained with all rows of \code{df}.
#'   \item `summary.model.performance` is a \code{data.frame} containing summary
#'   statistics across all model training iterations and pretreatments.
#'   See below for a description of the summary statistics provided.
#'   \item `model.performance` is a \code{data.frame} containing performance
#'   statistics for each iteration of model training separately (see below).
#'   \item `predictions` is a \code{data.frame} containing both reference and
#'   predicted values for each test set entry in each iteration of
#'   model training.
#'   \item `importance` is a \code{data.frame} containing variable importance
#'   results for each wavelength at each iteration of model training.
#'   If \code{model.method} is not "pls" or "rf", this list item is \code{NULL}.
#'   }
#'
#' `summary.model.performance` and `model.performance` \code{data.frames}
#' summary statistics include:
#' \itemize{
#'   \item Tuned parameters depending on the model algorithm:
#'   \itemize{
#'     \item \strong{Best.n.comp}, the best number of components
#'     \item \strong{Best.ntree}, the best number of trees in an RF model
#'     \item \strong{Best.mtry}, the best number of variables to include at
#'     every decision point in an RF model
#'     }
#'   \item \strong{RMSECV}, the root mean squared error of cross-validation
#'   \item \strong{R2cv}, the coefficient of multiple determination of
#'   cross-validation for PLSR models
#'   \item \strong{RMSEP}, the root mean squared error of prediction
#'   \item \strong{R2p}, the squared Pearson’s correlation between predicted and
#'   observed test set values
#'   \item \strong{RPD}, the ratio of standard deviation of observed test set
#'   values to RMSEP
#'   \item \strong{RPIQ}, the ratio of performance to interquartile difference
#'   \item \strong{CCC}, the concordance correlation coefficient
#'   \item \strong{Bias}, the average difference between the predicted and
#'   observed values
#'   \item \strong{SEP}, the standard error of prediction
#'   \item \strong{R2sp}, the squared Spearman’s rank correlation between
#'   predicted and observed test set values
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::rename(reference = DMC.oven,
#'                 unique.id = sample.id) %>%
#'   dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   test_spectra(
#'     train.data = .,
#'     tune.length = 3,
#'     num.iterations = 3,
#'     pretreatment = 1
#'   )
#' }
test_spectra <- function(train.data,
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
                         rf.variable.importance = lifecycle::deprecated()) {

  # Deprecate warnings ---------------------------
  handle_deprecations(
    function_name = "test_spectra",
    wavelengths = wavelengths,
    preprocessing = preprocessing,
    output.summary = output.summary,
    rf.variable.importance = rf.variable.importance
  )

  # Error handling ---------------------------
  validate_inputs(
    train.data = train.data,
    test.data = test.data,
    cv.scheme = cv.scheme,
    trial1 = trial1,
    trial2 = trial2,
    trial3 = trial3,
    model.method = model.method,
    tune.length = tune.length
  )

  # Handle cv.scheme data setup
  if (!is.null(cv.scheme)) {
    train.data <- trial1
  }

  # End error handling ---------------------------

  n.train <- nrow(train.data)
  n.test <- ifelse(is.null(test.data), 0, nrow(test.data))

  # Perform pretreatments on everything ---------------------------
  # Returns a list of data frames,
  # one for each transformation specified by pretreatment argument
  if (verbose) {
    cat("Pretreatment initiated.\n")
  }
  if (!is.null(cv.scheme)) {
    train.data <- rbind(trial1, trial2, trial3)
  }

  # Pretreat spectra ---------------------------
  methods.list <- c(
    "Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG",
    "SNVSG", "SGD1", "SG.D1W5", "SG.D1W11", "SG.D2W5", "SG.D2W11"
  )

  df.list <- pretreat_spectra(
    df = train.data,
    test.data = test.data,
    pretreatment = pretreatment
  )

  # If only one pretreatment, pretreat_spectra() outputs a data.frame,
  # not a list.
  # To simplify downstream use of pretreated spectra,
  # make this data.frame into a list with one item.
  if (length(pretreatment) == 1) {
    df.list <- list(df.list)
    names(df.list) <- methods.list[pretreatment]
  }

  # Training loop ---------------------------
  if (verbose) {
    cat("Training models...\n")
  }

  counter <- 0
  aggregated.results <- NULL
  
  for (i in pretreatment) {
    # This implementation allows for any combination of pretreatments
    # ex// pretreatment = c(1,4,8)
    counter <- counter + 1

    if (verbose) {
      cat(paste("Working on", methods.list[i], "\n", sep = " "))
    }

    # Extract preprocessed data using helper function --------
    processed.data <- process_pretreatment_data(
      df.list = df.list,
      methods.list = methods.list,
      i = i,
      n.train = n.train,
      n.test = n.test,
      cv.scheme = cv.scheme,
      trial1 = trial1,
      trial2 = trial2,
      trial3 = trial3
    )

    # Fit models for each pretreatment and output results ----------------------
    training.results.i <- train_spectra(
      df = processed.data$train.data,
      num.iterations = num.iterations,
      test.data = processed.data$test.data,
      k.folds = k.folds,
      proportion.train = proportion.train,
      tune.length = tune.length,
      model.method = model.method,
      cv.scheme = cv.scheme,
      stratified.sampling = stratified.sampling,
      trial1 = processed.data$trial1,
      trial2 = processed.data$trial2,
      trial3 = processed.data$trial3,
      split.test = split.test,
      verbose = verbose
    )

    # Aggregate results using helper function
    aggregated.results <- aggregate_pretreatment_results(
      training.results.i = training.results.i,
      methods.list = methods.list,
      i = i,
      pretreatment = pretreatment,
      counter = counter,
      existing.results = aggregated.results
    )
  }
  
  # Extract final results from aggregated results
  model.list <- aggregated.results$model.list
  summary.df <- aggregated.results$summary.df
  results.df <- aggregated.results$results.df
  predictions.df <- aggregated.results$predictions.df
  importance.df <- aggregated.results$importance.df
  
  # End of pretreatment loop ---------------------------
  rownames(summary.df) <- NULL
  rownames(results.df) <- NULL
  if (length(pretreatment) != 1) {
    names(model.list) <- methods.list[pretreatment]
  }

  if (model.method %in% c("pls", "rf")) {
    # Reformat importance.df
    # Some pretreatments trim the wavelengths,
    # so they do not return the full set of importance values.
    # If pivot_wider is used with each pretreatment separately,
    # the number of columns will not match.
    importance.df <- tidyr::pivot_wider(importance.df,
                                        names_from = .data$wavelength,
                                        values_from = .data$Overall)
  }

  results.list <- list(
    model = model.list,
    summary.model.performance = summary.df,
    model.performance = results.df,
    predictions = predictions.df,
    importance = importance.df
  )

  return(results.list)
}
