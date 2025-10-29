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
#' @importFrom dplyr select mutate summarize_all
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
  if (lifecycle::is_present(rf.variable.importance)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "train_spectra(rf.variable importance)",
      details = "Variable importance is now output by default when
      `model.method` is set to `pls` or `rf`."
    )
  }

  if (lifecycle::is_present(output.summary)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "train_spectra(output.summary)",
      details = "Summary is now default output alongside full results."
    )
  }

  if (lifecycle::is_present(return.model)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "train_spectra(return.model)",
      details = "Trained models are now default output alongside full results."
    )
  }

  # Error handling ---------------------------
  if (!(best.model.metric %in% c("RMSE", "Rsquared"))) {
    rlang::abort('best.model.metric must be either "RMSE" or "Rsquared"')
  }

  if (!(model.method %in% c("pls", "rf", "svmLinear", "svmRadial"))) {
    rlang::abort('model.method must be "pls", "rf", "svmLinear",
                 or "svmRadial"')
  }

  if (!rlang::has_name(df, "reference")) {
    rlang::abort('The training dataset must include a column named "reference"')
  }

  if (!is.null(test.data) && !(rlang::has_name(test.data, "reference"))) {
    rlang::abort('The test dataset must include a column named "reference"')
  }

  if (!(rlang::has_name(df, "unique.id"))) {
    rlang::abort('The training dataset must include a column named "unique.id"')
  }

  if (!is.null(test.data) && !(rlang::has_name(test.data, "unique.id"))) {
    rlang::abort('The test dataset must include a column named "unique.id"')
  }

  if (!is.null(cv.scheme)) {
    if (!(cv.scheme %in% c("CV1", "CV2", "CV0", "CV00"))) {
      rlang::abort('cv.scheme must be NULL, "CV0", "CV00", "CV1", or "CV2"')
    }
    # Set num.iterations based on cv.scheme
    if (cv.scheme == "CV0" || cv.scheme == "CV00") {
      num.iterations <- 1
    } # else use provided number of iterations
  }

  if (proportion.train > 1 || proportion.train < 0) {
    rlang::abort("'proportion.train' must be a number between 0 and 1")
  }

  df.colnames <- c(
    "Iteration", "ModelType", "RMSEp", "R2p", "RPD", "RPIQ", "CCC", "Bias",
    "SEP", "RMSEcv", "R2cv", "R2sp", "best.ncomp", "best.ntree", "best.mtry"
  )

  df.colnames.notype <- c(
    "Iteration", "RMSEp", "R2p", "RPD", "RPIQ", "CCC", "Bias",
    "SEP", "RMSEcv", "R2cv", "R2sp", "best.ncomp", "best.ntree", "best.mtry"
  )

  # Set seed ------------------------------
  set.seed(seed = seed)

  # Train model ---------------------------
  # Partition training and test sets
  # Random sampling occurs in the loop below
  if (is.null(test.data)) {
    partition.input.df <- df
  } else {
    partition.input.df <- test.data
  }

  if (!is.null(test.data) && !split.test) {
    # If fixed training and test sets provided but split.test = F
    # One iteration is the only option because there is only one
    # possible combination when the sets are fixed
    num.iterations <- 1
    data.train <- df
    data.test <- test.data
  }

  if (stratified.sampling && is.null(cv.scheme)) {
    # Stratified sampling to get representative sample of ground
    #   truth (reference column) values
    # Outputs list with n = num.iterations
    train.index <- caret::createDataPartition(
      y = partition.input.df$reference,
      p = proportion.train, times = num.iterations
      )
  }

  # set up results lists
  predictions.list <- vector("list", length = num.iterations)
  results.list  <- vector("list", length = num.iterations)
  importance.list  <- vector("list", length = num.iterations)

  for (i in 1:num.iterations) {
    # set seed, different for each iteration for random samples
    set.seed(i)

    if (!stratified.sampling && is.null(cv.scheme)) {
      # Random sample (not stratified)
        train.index <- sort(sample(
          x = seq_len(nrow(partition.input.df)),
          size = proportion.train * nrow(partition.input.df),
          replace = FALSE, prob = NULL
        ))
        if (is.null(test.data)) {
          # No test set provided
          data.train <- df[train.index, ]
          data.test <- df[-train.index, ]
        } else if (!is.null(test.data) && split.test) {
          # Test set provided and split randomly
          # Fixed training set + add proportion.train from test set pool
          #    to training set
          data.train <- rbind(df, test.data[train.index, ])
          data.test <- test.data[-train.index, ]
        }
      } else if (stratified.sampling && is.null(cv.scheme)) {
        # Stratified random sampling
        if (is.null(test.data)) {
          # No test set provided
          data.train <- df[train.index[[i]], ]
          data.test <- df[-train.index[[i]], ]
        } else if (!is.null(test.data) && split.test) {
          # Test set provided and split in a stratified random manner
          # Fixed training set + add proportion.train from test set pool to
          #   training set
          data.train <- rbind(df, test.data[train.index[[i]], ])
          data.test <- test.data[-train.index[[i]], ]
        }
      } else if (!is.null(cv.scheme)) {
      # cv.scheme present
      # Use selected cross-validation scheme
      formatted.lists <- format_cv(
        trial1 = trial1,
        trial2 = trial2,
        trial3 = trial3,
        cv.scheme = cv.scheme,
        stratified.sampling = stratified.sampling,
        proportion.train = proportion.train,
        seed = i,
        remove.genotype = TRUE
      )
      data.train <- formatted.lists$train.set
      data.test <- formatted.lists$test.set
    }

    train.ref.spectra <- data.train %>% dplyr::select(
      .data$reference,
      starts_with("X")
    )
    # Exclude reference column from test set
    test.spectra <- data.test %>% dplyr::select(starts_with("X"))

    if (num.iterations > 9) {
      cv.seeds <- c(1:num.iterations)
    } else {
      cv.seeds <- c(1:10)
    }

    # Tune hyperparameters with training data
    # Example// for 'pls', train hyperparameter "ncomps", where tune.length is
    #    number of ncomps tried
    if (model.method != "rf") {
      # 5-fold cross validation on training set
      cv.kfold <- caret::trainControl(
        method = "repeatedcv",
        number = k.folds,
        savePredictions = TRUE,
        seeds = cv.seeds
      )

      data.trained <- caret::train(reference ~ .,
        data = train.ref.spectra,
        method = model.method,
        tuneLength = tune.length,
        trControl = cv.kfold,
        metric = best.model.metric
      )
    }

    if (model.method == "pls") {
      # Extract best number of components
      best.ncomp <- data.trained$bestTune$ncomp
      best.ntree <- NA
      best.mtry <- NA
      # Put results as row in data frame
      predicted.values <- as.numeric(predict(data.trained$finalModel,
        newdata = as.matrix(test.spectra), # exclude reference column
        ncomp = best.ncomp
      ))
      R2cv <- pls::R2(data.trained$finalModel, ncomp = best.ncomp)[["val"]][2]
      RMSEcv <- pls::RMSEP(data.trained$finalModel,
        ncomp = best.ncomp
      )[["val"]][2]
    } else if (model.method == "svmLinear") {
      predicted.values <- as.numeric(predict(data.trained,
        newdata = as.matrix(test.spectra)
      ))
      best.ncomp <- NA
      best.ntree <- NA
      best.mtry <- NA
      R2cv <- NA
      RMSEcv <- NA
    } else if (model.method == "svmRadial") {
      predicted.values <- as.numeric(predict(data.trained,
        newdata = as.matrix(test.spectra)
      ))
      best.ncomp <- NA
      best.ntree <- NA
      best.mtry <- NA
      R2cv <- NA
      RMSEcv <- NA
    } else if (model.method == "rf") {
      cv.oob <- caret::trainControl(
        method = "oob",
        number = 5,
        savePredictions = TRUE,
        seeds = list(cv.seeds, cv.seeds)
      )
      data.trained <- caret::train(reference ~ .,
        data = train.ref.spectra,
        method = model.method,
        tuneLength = tune.length,
        trControl = cv.oob,
        metric = best.model.metric,
        importance = TRUE
      )
      # Extract best ntree and mtry
      best.ncomp <- NA
      best.ntree <- data.trained$finalModel$ntree
      best.mtry <- data.trained$finalModel$mtry

      predicted.values <- as.numeric(predict(data.trained$finalModel,
        newdata = as.matrix(test.spectra),
        ntree = best.ntree,
        mtry = best.mtry
      ))

      R2cv <- NA
      RMSEcv <- NA
    }

    # Variable importance ---------------------------
    # Can only be performed for pls and rf model types
    # Each row contains iteration number followed by model type and importance
    #   value of each wavelength.
    if (model.method %in% c("pls", "rf")) {
      importance.df.i <- cbind(
        "Iteration" = i, "ModelType" = model.method,
        caret::varImp(data.trained$finalModel)
      ) %>%
        tibble::rownames_to_column(var = "wavelength")
      rownames(importance.df.i) <- NULL
    } else {
      importance.df.i <- NULL
    }

    # Get model performance statistics ---------------------------
    reference.values <- data.test$reference
    # Squared Spearman's rank correlation
    R2sp <- cor(predicted.values, reference.values, method = "spearman")**2
    spectacles.df.i <- as.data.frame(t(spectacles::postResampleSpectro(
      pred = predicted.values,
      obs = reference.values
    )))
    results.df.i <- cbind(
      i, spectacles.df.i,
      RMSEcv, R2cv, R2sp,
      best.ncomp, best.ntree, best.mtry
    )
    colnames(results.df.i) <- df.colnames.notype

    # Compile predictions ---------------------------
    predictions.df.i <- cbind(i, model.method, data.test$unique.id,
                              reference.values, predicted.values)
    colnames(predictions.df.i) <- c("Iteration", "ModelType", "unique.id",
                                    "reference", "predicted")

    predictions.list[[i]] <- predictions.df.i
    results.list[[i]]  <- results.df.i
    importance.list[[i]]  <- importance.df.i

  } # End of loop

  #
  predictions.df <- dplyr::bind_rows(predictions.list)
  results.df     <- dplyr::bind_rows(results.list)
  importance.df  <- dplyr::bind_rows(importance.list)


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
  if (model.method == "pls") {
    # Format df for plsr() function
    df.plsr <- df %>%
      dplyr::select(-starts_with("X")) %>%
      as.data.frame()
    df.plsr$spectra <- df %>%
      dplyr::select(starts_with("X")) %>%
      as.matrix()
    full.model <- pls::plsr(reference ~ spectra,
      ncomp = tune.length,
      data = df.plsr
    )
  }
  if (model.method == "rf") {
    df.rf <- df %>% dplyr::select(.data$reference, starts_with("X"))
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
