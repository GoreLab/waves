#' @title Train a model based predict reference values with spectral data
#' @name TrainSpectralModel
#' @description Trains spectral prediction models using one of several algorithms and sampling
#' procedures.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams FormatCV
#' @param df \code{data.frame} object. First column contains unique identifiers,
#'   second contains reference values, followed by spectral columns. Include no
#'   other columns to right of spectra! Column names of spectra must start with
#'   "X" and reference column must be named "reference"
#' @param num.iterations Number of training iterations to perform
#' @param test.data \code{data.frame} with same specifications as \code{df}. Use
#'   if specific test set is desired for hyperparameter tuning. If \code{NULL},
#'   function will automatically train with a stratified sample of 70\%. Default
#'   is \code{NULL}.
#' @param k.folds Number indicating the number of folds for k-fold cross-validation
#'   during model training. Default is 5.
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
#' @param verbose If \code{TRUE}, the number of rows removed through filtering
#'   will be printed to the console. Default is \code{TRUE}.
#'
#' @return list of the following:
#' \enumerate{
#'   \item \code{model} is a model object trained with all rows of \code{df}.
#'   \item \code{summary.model.performance} is a \code{data.frame} with model performance
#'   statistics in summary format (2 rows, one with mean and one with standard deviation of
#'   all training iterations).
#'   \item \code{full.model.performance} is a \code{data.frame} with model performance
#'   statistics in long format (number of rows = \code{num.iterations})
#'   \item \code{predictions} is a \code{data.frame} containing predicted values for each
#'   test set entry at each iteration of model training.
#'   \item \code{importance} is a \code{data.frame} that contains variable importance for
#'   each wavelength. Only available for \code{model.method} options "rf" and "pls".
#' Included summary statistics:
#' \itemize{
#'   \item Tuned parameters depending on the model algorithm:
#'   \itemize{
#'     \item \strong{Best.n.comp}, the best number of components
#'     \item \strong{Best.ntree}, the best number of trees in an RF model
#'     \item \strong{Best.mtry}, the best number of variables to include at every decision point in an RF model
#'     }
#'   \item \strong{RMSECV}, the root mean squared error of cross-validation
#'   \item \strong{R2cv}, the coefficient of multiple determination of cross-validation for PLSR models
#'   \item \strong{RMSEP}, the root mean squared error of prediction
#'   \item \strong{R2p}, the squared Pearson’s correlation between predicted and observed test set values
#'   \item \strong{RPD}, the ratio of standard deviation of observed test set values to RMSEP
#'   \item \strong{RPIQ}, the ratio of performance to interquartile difference
#'   \item \strong{CCC}, the concordance correlation coefficient
#'   \item \strong{Bias}, the average difference between the predicted and observed values
#'   \item \strong{SEP}, the standard error of prediction
#'   \item \strong{R2sp}, the squared Spearman’s rank correlation between predicted and observed test set values
#' }
#'
#' @importFrom caret createDataPartition trainControl train createResample
#' @importFrom dplyr select mutate summarize_all
#' @importFrom tidyselect starts_with everything
#' @importFrom magrittr %>% %<>%
#' @importFrom stats cor predict sd
#' @importFrom spectacles postResampleSpectro
#' @importFrom randomForest importance
#' @importFrom rlang .data
#' @importFrom pls R2 RMSEP mvrValstats MSEP
#'
#' @export TrainSpectralModel
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::filter(study.name == "C16Mcal") %>%
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::select(sample.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   TrainSpectralModel(
#'     df = .,
#'     tune.length = 3,
#'     num.iterations = 3,
#'     best.model.metric = "RMSE",
#'     stratified.sampling = TRUE
#'   )
#' }
TrainSpectralModel <- function(df,
                               num.iterations,
                               test.data = NULL,
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
  #### Error handling ####
  if (!(best.model.metric %in% c("RMSE", "Rsquared"))) {
    stop('best.model.metric must be either "RMSE" or "Rsquared"')
  }

  if (!(model.method %in% c("pls", "rf", "svmLinear", "svmRadial"))) {
    stop('model.method must be "pls", "rf", "svmLinear", or "svmRadial"')
  }

  if (rf.variable.importance & !model.method %in% c("rf", "pls")) {
    stop('model.method must be "rf" or "pls" if variable.importance is TRUE.
         This option is not available for SVM models at this time.')
  }

  if (!("reference" %in% colnames(df))) {
    stop('The training dataset must include a column named "reference"')
  }

  if (!is.null(test.data) & !("reference" %in% colnames(test.data))) {
    stop('The test dataset must include a column named "reference"')
  }

  if (!is.null(cv.scheme)) {
    if (!(cv.scheme %in% c("CV1", "CV2", "CV0", "CV00"))) {
      stop('cv.scheme must be NULL, "CV0", "CV00", "CV1", or "CV2"')
    }
    # Set num.iterations based on cv.scheme
    if (cv.scheme == "CV0" | cv.scheme == "CV00") {
      num.iterations <- 1
    } # else use provided number of iterations
  }

  df.colnames <- c(
    "Iteration", "ModelType", "RMSEp", "R2p", "RPD", "RPIQ", "CCC", "Bias",
    "SEP", "RMSEcv", "R2cv", "R2sp", "best.ncomp", "best.ntree", "best.mtry"
  )

  # Train model

  # Partition training and test sets
  if (is.null(test.data)) {
    # No separate test set provided
    if (stratified.sampling) {
      # Stratified sampling to get representative sample of ground truth (reference column) values
      # Outputs list with n = num.iterations
      train.index <- caret::createDataPartition(df$reference, p = 0.7, times = num.iterations)
    } else if (!stratified.sampling) {
      # Random sample (not stratified)
      train.index <- caret::createResample(df, p = 0.7, times = num.iterations)
    }
  } else if (!is.null(test.data)) {
    # If fixed training and test sets provided
    if (split.test) {
      # Fixed training set + add 70% of samples from test set pool to training set
      train.index <- caret::createDataPartition(test.data$reference,
        p = 0.7, times = num.iterations
      )
    } else if (!split.test) {
      # If fixed training and test sets provided but split.test = F
      num.iterations <- 1 # only one possible combination because the sets are fixed
      data.train <- df
      data.test <- test.data
    }
  }

  for (i in 1:num.iterations) {
    # set seed, different for each iteration for random samples
    set.seed(i)

    if (is.null(cv.scheme)) {
      if (split.test & !is.null(test.data)) {
        # Fixed training set + add 70% of samples from test set pool to training set
        data.train <- rbind(df, test.data[train.index[[i]], ])
        data.test <- test.data[-train.index[[i]], ]
      }
      else if (!split.test & is.null(test.data)) {
        # If fixed training and test sets provided but split.test = F
        data.train <- df[train.index[[i]], ]
        data.test <- df[-train.index[[i]], ]
      }

      else if (!is.null(cv.scheme)) {
        # cv.scheme present
        # Use selected cross-validation scheme
        formatted.lists <- FormatCV(
          trial1 = trial1,
          trial2 = trial2,
          trial3 = trial3,
          cv.scheme = cv.scheme,
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
      # Example// for 'pls', train hyperparameter "ncomps", where tune.length is number of ncomps tried
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
        # Put results as row in data frame
        predicted.values <- as.numeric(predict(data.trained$finalModel,
          newdata = as.matrix(test.spectra), # exclude reference column
          ncomp = best.ncomp
        ))
        # Extract best number of components
        best.ncomp <- data.trained$bestTune$ncomp
        best.ntree <- NA
        best.mtry <- NA
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
        predicted.values <- as.numeric(predict(data.trained$finalModel,
          newdata = as.matrix(test.spectra),
          ntree = best.ntree,
          mtry = best.mtry
        ))

        best.ncomp <- NA
        best.ntree <- data.trained$finalModel$ntree
        best.mtry <- data.trained$finalModel$mtry
        R2cv <- NA
        RMSEcv <- NA
      }


      # Variable importance can only be performed for pls and rf model types.
      # Each row contains iteration number followed by importance value of each wavelength.
      if (model.method %in% c("pls", "rf")) {
        importance.df.i <- cbind(i, t(importance(data.trained$finalModel, type = 1)))
      }

      # Get model performance statistics
      reference.values <- data.test$reference
      R2sp <- cor(predicted.values, reference.values, method = "spearman")**2 # Squared Spearman's rank correlation
      results.df.i <- cbind(
        t(i, spectacles::postResampleSpectro(predicted.values, reference.values)),
        RMSEcv, R2cv, R2sp, best.ncomp, best.ntree, best.mtry
      )
      colnames(results.df.i) <- df.colnames

      # Compile predictions
      predictions.df.i <- cbind(i, model.method, data.test$unique.id, reference.values, predicted.values)
      colnames(predictions.df.i) <- c("Iteration", "ModelType", "unique.id", "reference", "predicted")

      if (i == 1) {
        predictions.df <- predictions.df.i
        results.df <- results.df.i
        importance.df <- ifelse(model.method %in% c("pls", "rf"), importance.df.i, NULL)
      } else {
        predictions.df <- rbind(predictions.df, predictions.df.i)
        results.df <- rbind(results.df, results.df.i)
        importance.df <- ifelse(model.method %in% c("pls", "rf"),
          rbind(importance.df, importance.df.i),
          NULL
        )
      }
    }
  } # end loop

  #' @description Get the mode of a set of numbers. Used in getting summary of results
  #' within [TrainSpectralModel()]
  #' @param vector.input The mode of this vector of numbers will be calculated by this function
  #' @return mode of the numbers in `vector.input`
  #' @export
  #' @keywords internal
  getmode <- function(vector.input) {
    as.matrix(vector.input)
    unique.vector <- unique(vector.input)
    return(unique.vector[which.max(tabulate(match(vector.input, unique.vector)))])
  }

  # Create summary data.frame
  summary.df <- rbind(
    summarize_all(results.df, .funs = mean),
    summarize_all(results.df, .funs = sd, na.rm = TRUE),
    summarize_all(results.df, .funs = getmode)
  ) %>%
    mutate(
      Model_type = model.method,
      Summary_type = c("mean", "sd", "mode")
    ) %>%
    # Get rid of iteration column and reorder remaining
    dplyr::select(
      .data$Summary_type, .data$ModelType, .data$RMSEp:.data$R2sp,
      .data$best.ncomp, .data$best.ntree, .data$best.mtry
    )

  # Stitch on Model_type column later so doesn't interfere with mean calculations for summary
  results.df$Model_type <- model.method
  results.df %<>%
    dplyr::select(all_of(df.colnames))

  # Create model with all input data (not just 70%). Results will give an idea of this model's performance,
  #     but they will have been generated with only subsets of the data.
  if (verbose) cat("\nReturning model...\n")
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
  if (model.method == "svmLinear" | model.method == "svmRadial") {
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
    predictions = predictions.df,
    importance = importance.df
  ))
}
