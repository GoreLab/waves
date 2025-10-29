#' @title Train a model based predict reference values with spectral data
#' @name train_spectra
#' @description Trains spectral prediction models using one of several
#' algorithms and sampling procedures.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams format_cv
#' @param df \code{data.frame} object. First column contains unique identifiers,
#'   second contains reference values, followed by spectral columns. Include no
#'   other columns to right of spectra! Column names of spectra must start with
#'   "X" and reference column must be named "reference"
#' @param num.iterations Number of training iterations to perform
#' @param test.data \code{data.frame} with same specifications as \code{df}. Use
#'   if specific test set is desired for hyperparameter tuning. If \code{NULL},
#'   function will automatically train with a stratified sample of 70\%. Default
#'   is \code{NULL}.
#' @param k.folds Number indicating the number of folds for k-fold
#' cross-validation during model training. Default is 5.
#' @param tune.length Number delineating search space for tuning of the PLSR
#'   hyperparameter \code{ncomp}. Must be set to 5 when using the random forest
#'   algorithm (\code{model.method == rf}). Default is 50.
#' @param model.method Model type to use for training. Valid options include:
#'   \itemize{ \item "pls": Partial least squares regression (Default) \item
#'   "rf": Random forest \item "svmLinear": Support vector machine with linear
#'   kernel \item "svmRadial": Support vector machine with radial kernel }
#' @param best.model.metric Metric used to decide which model is best. Must be
#'   either "RMSE" or "Rsquared"
#' @param stratified.sampling If \code{TRUE}, training and test sets will be
#'   selected using stratified random sampling. This term is only used if
#'   \code{test.data == NULL}. Default is \code{TRUE}.
#' @param split.test boolean that allows for a fixed training set and a split
#'   test set. Example// train model on data from two breeding programs and a
#'   stratified subset (70\%) of a third and test on the remaining samples
#'   (30\%)  of the third. If \code{FALSE}, the entire provided test set
#'   \code{test.data} will remain as a testing set or if none is provided, 30\%
#'   of the provided \code{train.data} will be used for testing. Default is
#'   \code{FALSE}.
#' @param seed Integer to be used internally as input for \code{set.seed()}.
#' Only used if \code{stratified.sampling = TRUE}. In all other cases, seed
#' is set to the current iteration number. Default is 1.
#' @param verbose If \code{TRUE}, the number of rows removed through filtering
#'   will be printed to the console. Default is \code{TRUE}.
#' @param save.model DEPRECATED \code{save.model = FALSE} is no
#'   longer supported; this function will always return a saved model.
#' @param rf.variable.importance DEPRECATED
#'   \code{rf.variable.importance = FALSE} is no longer supported; variable
#'   importance results are always returned if the \code{model.method} is
#'   set to `pls` or `rf`.
#' @param output.summary DEPRECATED \code{output.summary = FALSE}
#'   is no longer supported; a summary of output is always returned alongside
#'   the full performance statistics.
#' @param return.model DEPRECATED \code{return.model = FALSE}
#'   is no longer supported; a trained model object is always returned
#'   alongside the full performance statistics and summary.
#'
#' @return list of the following:
#' \enumerate{
#'   \item \code{model} is a model object trained with all rows of \code{df}.
#'   \item \code{summary.model.performance} is a \code{data.frame} with model
#'   performance statistics in summary format (2 rows, one with mean and one
#'   with standard deviation of all training iterations).
#'   \item \code{full.model.performance} is a \code{data.frame} with model
#'   performance statistics in long format
#'   (number of rows = \code{num.iterations})
#'   \item \code{predictions} is a \code{data.frame} containing predicted values
#'   for each test set entry at each iteration of model training.
#'   \item \code{importance} is a \code{data.frame} that contains variable
#'   importance for each wavelength. Only available for \code{model.method}
#'   options "rf" and "pls".
#'   }
#' Included summary statistics:
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
#' @importFrom caret createDataPartition trainControl train createResample varImp
#' @importFrom dplyr select mutate summarise across bind_rows
#' @importFrom tidyselect starts_with everything all_of
#' @importFrom magrittr %>% %<>%
#' @importFrom tibble rownames_to_column
#' @importFrom stats cor predict sd
#' @importFrom spectacles postResampleSpectro
#' @importFrom randomForest importance
#' @importFrom rlang .data abort has_name
#' @importFrom pls R2 RMSEP mvrValstats MSEP
#' @importFrom lifecycle deprecated
#'
#' @export train_spectra
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::filter(study.name == "C16Mcal") %>%
#'   dplyr::rename(reference = DMC.oven,
#'                 unique.id = sample.id) %>%
#'   dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   train_spectra(
#'     df = .,
#'     tune.length = 3,
#'     num.iterations = 3,
#'     best.model.metric = "RMSE",
#'     stratified.sampling = TRUE
#'   ) %>%
#'   summary()
#' }
train_spectra <- function(df,
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
                          seed = 1,
                          verbose = TRUE,
                          save.model = deprecated(),
                          rf.variable.importance = deprecated(),
                          output.summary = deprecated(),
                          return.model = deprecated()) {

  # Deprecate warnings ---------------------------
  handle_deprecations(
    function_name = "train_spectra",
    rf.variable.importance = rf.variable.importance,
    output.summary = output.summary,
    return.model = return.model
  )

  # Error handling ---------------------------
  validate_inputs(
    train.data = df,
    test.data = test.data,
    cv.scheme = cv.scheme,
    trial1 = trial1,
    trial2 = trial2,
    trial3 = trial3,
    model.method = model.method,
    tune.length = tune.length,
    best.model.metric = best.model.metric,
    proportion.train = proportion.train
  )

  # Handle cv.scheme iteration adjustment
  if (!is.null(cv.scheme) && (cv.scheme == "CV0" || cv.scheme == "CV00")) {
    num.iterations <- 1
  }

  # Setup for training ---------------------------
  if (!is.null(test.data) && !split.test) {
    # If fixed training and test sets provided but split.test = FALSE
    # One iteration is the only option because there is only one
    # possible combination when the sets are fixed
    num.iterations <- 1
  }

  # Determine partition input for stratified sampling
  partition.input.df <- if (is.null(test.data)) df else test.data

  # Pre-calculate stratified training indices if needed
  train.index <- NULL
  if (stratified.sampling && is.null(cv.scheme)) {
    train.index <- caret::createDataPartition(
      y = partition.input.df$reference,
      p = proportion.train, 
      times = num.iterations
    )
  }

  # Pre-calculate column indices and other optimizations
  spectra.cols <- which(startsWith(names(df), "X"))
  ref.col <- which(names(df) == "reference")
  ref.spectra.cols <- c(ref.col, spectra.cols)  # Pre-combine for repeated use
  
  if (!is.null(test.data)) {
    test.spectra.cols <- which(startsWith(names(test.data), "X"))
  }
  
  # Pre-calculate CV seeds
  cv.seeds <- if (num.iterations > 9) c(1:num.iterations) else c(1:10)

  # Set up results storage
  predictions.list <- vector("list", length = num.iterations)
  results.list <- vector("list", length = num.iterations)
  importance.list <- vector("list", length = num.iterations)

  # Main training loop ---------------------------
  set.seed(seed)
  
  for (i in 1:num.iterations) {
    # Partition data for this iteration
    partitioned.data <- partition_data(
      df = df,
      test.data = test.data,
      iteration = i,
      num.iterations = num.iterations,
      stratified.sampling = stratified.sampling,
      proportion.train = proportion.train,
      split.test = split.test,
      cv.scheme = cv.scheme,
      trial1 = trial1,
      trial2 = trial2,
      trial3 = trial3,
      train.index = train.index
    )
    
    data.train <- partitioned.data$train
    data.test <- partitioned.data$test

    # Prepare training data
    train.ref.spectra <- data.train[, ref.spectra.cols, drop = FALSE]
    names(train.ref.spectra)[1] <- "reference"  # Ensure reference column name
    
    # Prepare test data
    if (!is.null(test.data)) {
      test.spectra <- as.matrix(data.test[, test.spectra.cols, drop = FALSE])
    } else {
      test.spectra <- as.matrix(data.test[, spectra.cols, drop = FALSE])
    }

    # Train model and get predictions
    model.results <- train_individual_model(
      train.ref.spectra = train.ref.spectra,
      test.spectra = test.spectra,
      model.method = model.method,
      tune.length = tune.length,
      k.folds = k.folds,
      best.model.metric = best.model.metric,
      cv.seeds = cv.seeds
    )

    # Calculate performance statistics
    performance.results <- calculate_performance(
      predicted.values = model.results$predictions,
      reference.values = data.test$reference,
      iteration = i,
      model.method = model.method,
      R2cv = model.results$R2cv,
      RMSEcv = model.results$RMSEcv,
      best.ncomp = model.results$best.ncomp,
      best.ntree = model.results$best.ntree,
      best.mtry = model.results$best.mtry,
      importance.df = model.results$importance,
      unique.ids = data.test$unique.id
    )

    # Store results
    predictions.list[[i]] <- performance.results$predictions
    results.list[[i]] <- performance.results$results
    importance.list[[i]] <- performance.results$importance
  }

  # Combine results from all iterations ---------------------------
  df.colnames <- c(
    "Iteration", "ModelType", "RMSEp", "R2p", "RPD", "RPIQ", "CCC", "Bias",
    "SEP", "RMSEcv", "R2cv", "R2sp", "best.ncomp", "best.ntree", "best.mtry"
  )

  predictions.df <- dplyr::bind_rows(predictions.list)
  results.df <- dplyr::bind_rows(results.list)
  
  # Remove NULL values before binding (for model types that don't support importance)
  importance.list <- Filter(Negate(is.null), importance.list)
  if (length(importance.list) > 0) {
    importance.df <- dplyr::bind_rows(importance.list)
  } else {
    importance.df <- NULL
  }


  # Create summary data.frame ---------------------------
  summary.df <- rbind(
    dplyr::summarise(results.df, dplyr::across(dplyr::everything(), mean)),
    dplyr::summarise(results.df, dplyr::across(dplyr::everything(), sd, na.rm = TRUE)),
    dplyr::summarise(results.df, dplyr::across(dplyr::everything(), get_mode))
  ) %>%
    mutate(
      ModelType = model.method,
      SummaryType = c("mean", "sd", "mode")
    ) %>%
    # Get rid of iteration column and reorder remaining
    dplyr::select(
      .data$SummaryType, .data$ModelType, .data$RMSEp:.data$R2sp,
      .data$best.ncomp, .data$best.ntree, .data$best.mtry
    )

  # Stitch on ModelType column later so doesn't interfere with
  # mean calculations for summary
  results.df$ModelType <- model.method
  results.df <- results.df %>%
    dplyr::select(all_of(df.colnames))

  # Create model with all input data (not just 70%). Results will give an idea
  # of this model's performance, but they will have been generated with
  # only subsets of the data.
  if (verbose) cat("Returning model...\n")
  
  # Set up cross-validation for final model (needed for SVM models)
  cv.kfold <- create_cv_control(k.folds = k.folds, cv.seeds = cv.seeds)
  
  if (model.method == "pls") {
    # Format df for plsr() function using optimized indexing
    df.plsr <- as.data.frame(df[, -spectra.cols, drop = FALSE])
    df.plsr$spectra <- as.matrix(df[, spectra.cols, drop = FALSE])
    full.model <- pls::plsr(reference ~ spectra,
      ncomp = tune.length,
      data = df.plsr
    )
  }
  if (model.method == "rf") {
    df.rf <- df[, ref.spectra.cols, drop = FALSE]
    names(df.rf)[1] <- "reference"  # Ensure reference column name
    full.model <- randomForest::randomForest(reference ~ .,
      data = df.rf,
      importance = FALSE,
      ntree = tune.length
    )
  }
  if (model.method == "svmLinear" || model.method == "svmRadial") {
    full.model <- caret::train(reference ~ .,
      data = df,
      method = model.method,
      tuneLength = tune.length,
      trControl = cv.kfold,
      metric = best.model.metric
    )
  }

  return(list(
    model = full.model,
    summary.model.performance = summary.df,
    model.performance = results.df,
    predictions = as.data.frame(predictions.df),
    importance = importance.df
  ))
}
