#' @title Test the performance of spectral models
#' @name TestModelPerformance
#' @description Wrapper that trains models based spectral data to predict
#'   reference values and reports model performance statistics
#'
#' @details Calls \code{\link{DoPreprocessing}}, \code{\link{FormatCV}},
#' and \code{\link{TrainSpectralModel}} functions.
#'
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams FormatCV
#' @inheritParams TrainSpectralModel
#' @inheritParams DoPreprocessing
#' @param train.data \code{data.frame} object of spectral data for input into a
#'   spectral prediction model. First column contains unique identifiers, second
#'   contains reference values, followed by spectral columns. Include no other
#'   columns to right of spectra! Column names of spectra must start with "X"
#'   and reference column must be named "reference".
#' @param preprocessing `r lifecycle::badge("deprecated")` please use
#'   \code{pretreatment} to specify the specific pretreatment to test. For behavior
#'   identical to that of \code{preprocessing = TRUE}, set
#'   \code{pretreatment = 1:13}`.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @return \code{list} of 5 objects:
#'   \enumerate{
#'   \item model.list is a \code{list} of trained model objects, one for each pretreatment
#'   method specified by the \code{pretreatment} argument. Each model is trained with all
#'   rows of \code{df}.
#'   \item summary.model.performance is a \code{data.frame} containing summary statistics
#'   across all model training iterations and pretreatments.
#'   \item model.performance is a \code{data.frame} containing performance statistics for
#'   each iteration of model training separately.
#'   \item predictions is a \code{data.frame} containing both reference and predicted
#'   values for each test set entry in each iteration of model training.
#'   \item importance is a \code{data.frame} containing variable importance results for
#'   each wavelength at each iteration of model training. If \code{model.method} is not
#'   "pls" or "rf", this list item is \code{NULL}.
#'   }
#'
#' \code{data.frame} with model performance statistics in summary format
#'   (2 rows, one with mean and one with standard deviation of all training
#'   iterations) or in long format (number of rows = num.iterations).
#'   \strong{Note} if \code{preprocessing = TRUE}, only the first mean of
#'   summary statistics for all iterations of training are provided for each
#'   technique.
#' Included summary statistics:
#' \itemize{
#'   \item Tuned parameters depending on the model algorithm:
#'   \itemize{
#'     \item \strong{Best.n.comp}, the best number of components
#'     \item \strong{Best.ntree}, the best number of trees in an RF model
#'     \item \strong{Best.mtry}, the best number of variables to include at every
#'     decision point in an RF model
#'     }
#'   \item \strong{RMSECV}, the root mean squared error of cross-validation
#'   \item \strong{R2cv}, the coefficient of multiple determination of cross-validation
#'   for PLSR models
#'   \item \strong{RMSEP}, the root mean squared error of prediction
#'   \item \strong{R2p}, the squared Pearson’s correlation between predicted and
#'   observed test set values
#'   \item \strong{RPD}, the ratio of standard deviation of observed test set values
#'   to RMSEP
#'   \item \strong{RPIQ}, the ratio of performance to interquartile difference
#'   \item \strong{CCC}, the concordance correlation coefficient
#'   \item \strong{Bias}, the average difference between the predicted and observed
#'   values
#'   \item \strong{SEP}, the standard error of prediction
#'   \item \strong{R2sp}, the squared Spearman’s rank correlation between predicted
#'   and observed test set values
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::rename(unique.id = sample.id) %>%
#'   dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   TestModelPerformance(
#'     train.data = .,
#'     tune.length = 3,
#'     num.iterations = 3,
#'     pretreatment = 1
#'   )
#' }
TestModelPerformance <- function(train.data,
                                 num.iterations,
                                 test.data = NULL,
                                 pretreatment = 1,
                                 k.folds = 5,
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
  # Error handling ---------------------------
  if (!is.null(cv.scheme)) {
    if (is.null(trial1)) {
      stop("trial1 must be provided if using cv.scheme")
    }
    if (is.null(trial2)) {
      stop("trial2 must be provided if using cv.scheme")
    }
    if (sum(colnames(trial1) != colnames(trial2)) > 0) {
      stop("Column names must match for trial1 and trial2 if using cv.scheme")
    }
    if (!is.null(trial3) & sum(colnames(trial1) != colnames(trial3)) > 0) {
      stop("Column names must match for trial1, trial2, and trial3 if using cv.scheme and including trial3")
    }
    train.data <- trial1
  }

  num.col.before.reference <- ifelse(!is.null(cv.scheme), 2, 1)

  if (nrow(train.data) != nrow(na.omit(train.data))) {
    stop("Training data cannot contain missing values.")
  }

  if (!is.null(test.data) && (nrow(test.data) != nrow(na.omit(test.data)))) {
    stop("Test data cannot contain missing values. \nEither omit missing values or exclude training data (
         set as NULL).")
  }

  if (variable.importance & !model.method %in% c("pls", "rf")) {
    stop('model.method must be "rf" or "pls" if variable.importance is TRUE')
  }

  if (model.method == "rf" & tune.length > 5) {
    stop("The waves implementation of the random forest algorithm uses oob cross-validation for model training
         and requires a tune length of 5.")
  }

  # End error handling ---------------------------

  n.train <- nrow(train.data)
  n.test <- ifelse(is.null(test.data), 0, nrow(test.data))

  # Perform pretreatments on everything ---------------------------
  # Returns a list of data frames, one for each transformation specified by pretreatment argument
  if (verbose) {
    cat("Preprocessing initiated.\n")
  }
  if (!is.null(cv.scheme)) {
    train.data <- rbind(trial1, trial2, trial3)
  }
  df.list <- DoPreprocessing(
    df = train.data, test.data = test.data,
    pretreatment = pretreatment
  )

  # Training loop ---------------------------
  if (verbose) {
    cat("Training models...\n")
  }

  methods.list <- c(
    "Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG",
    "SNVSG", "SGD1", "SG.D1W5", "SG.D1W11", "SG.D2W5", "SG.D2W11"
  )

  counter <- 0
  for (i in pretreatment) {
    # This implementation allows for any combination of pretreatments
    # ex// pretreatment = c(1,4,8)
    counter <- counter + 1

    if (verbose) {
      cat(paste("Working on", methods.list[i], "\n", sep = " "))
    }

    # Extract preprocessed data ---------------------------
    # df.list contains named data.frames transformed by the requested methods (only).
    # To access a specific method, use df.list[[methods.list[i]]].
    # This will call the preprocessed data.frame by name from the transformed list.
    # Then extract the test dataset from full processed data frame
    processed.train.data <- df.list[[methods.list[i]]][1:n.train, ]
    if (n.test == 0) {
      processed.test.data <- NULL
    } else {
      processed.test.data <- df.list[[methods.list[i]]][(n.train + 1):(n.train + n.test), ]
    }

    if (!is.null(cv.scheme)) {
      processed.trial1 <- df.list[[methods.list[i]]][1:nrow(trial1), ]
      processed.trial2 <- df.list[[methods.list[i]]][(nrow(trial1) + 1):(nrow(trial1) + nrow(trial2)), ]
      processed.trial3 <- df.list[[methods.list[i]]][(nrow(trial1) + nrow(trial2) + 1):nrow(train.data), ]
    }

    # Fit models for each pretreatment and output results ---------------------------
    training.results.i <- TrainSpectralModel(
      df = processed.train.data,
      num.iterations = num.iterations,
      test.data = processed.test.data,
      k.folds = k.folds,
      tune.length = tune.length,
      model.method = model.method,
      stratified.sampling = stratified.sampling,
      trial1 = processed.trial1,
      trial2 = processed.trial2,
      trial3 = processed.trial3,
      split.test = split.test,
      verbose = verbose
    )

    if (length(pretreatment > 1)) {
      # Reformat summary statistics data.frame so multiple pretreatments can be stacked
      # Put pretreatment name in first column followed by performance statistics
      # Append Summary_type (mean, sd, mode) to statistic name to flatten into a single row
      summary.i <- training.results.i$summary.model.performance %>%
        tidyr::pivot_longer(cols = .data$RMSEp:.data$best.mtry) %>%
        pivot_wider(
          id_cols = .data$Summary_type,
          names_from = c(.data$name, .data$Summary_type), names_sep = "."
        )
    } else {
      summary.i <- training.results.i$summary.model.performance
    }

    if (counter == 1) {
      # Set up results compilations in first iteration
      model.list <- ifelse(length(pretreatment) > 1,
        list(training.results.i$model),
        training.results.i$model
      )
      summary.df <- c(Pretreatment = methods.list[i], summary.i)
      results.df <- c(Pretreatment = methods.list[i], training.results.i$model.performance)
      predictions.df <- c(Pretreatment = methods.list[i], training.results.i$predictions)
      importance.df <- c(Pretreatment = methods.list[i], training.results.i$importance)
    } else {
      # Add new results to existing objects
      model.list <- append(model.list, list(training.results.i$model))
      summary.df <- rbind(summary.df, c(Pretreatment = methods.list[i], summary.i))
      results.df <- rbind(
        results.df,
        c(Pretreatment = methods.list[i], training.results.i$model.performance)
      )
      predictions.df <- rbind(
        predictions.df,
        c(Pretreatment = methods.list[i], training.results.i$predictions)
      )
      importance.df <- rbind(
        importance.df,
        c(Pretreatment = methods.list[i], training.results.i$importance)
      )
    }
  } # End of loop ---------------------------

  results.list <- list(
    model = model.list,
    summary.model.performance = summary.df,
    model.performance = results.df,
    predictions = predictions.df,
    importance = importance.df
  )

  return(results.list)
}
