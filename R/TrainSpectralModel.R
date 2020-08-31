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
#' @param tune.length Number delineating search space for tuning of the PLSR
#'   hyperparameter \code{ncomp}. Default is 50.
#' @param model.method Model type to use for training. Valid options include:
#'   \itemize{ \item "pls": Partial least squares regression (Default) \item
#'   "rf": Random forest \item "svmLinear": Support vector machine with linear
#'   kernel \item "svmRadial": Support vector machine with radial kernel }
#' @param output.summary boolean that controls function output. \itemize{ \item
#'   If \code{TRUE}, a summary df will be output (1st row = means, 2nd row =
#'   standard deviations). Default is \code{TRUE}. \item If \code{FALSE}, entire
#'   results data frame will be output }
#' @param return.model boolean that, if \code{TRUE}, causes the function to
#'   return the trained model in addition to the results data frame. \itemize{
#'   \item If \code{TRUE}, function return list of \code{[model, results]}.
#'   \item If \code{FALSE}, returns results data frame without model. Default is
#'   \code{FALSE}. }
#' @param best.model.metric Metric used to decide which model is best. Must be
#'   either "RMSE" or "Rsquared"
#' @param rf.variable.importance boolean that: \itemize{ \item If \code{TRUE},
#'   \code{model.method} must be set to "rf". Returns a list with a model
#'   performance \code{data.frame} and a second \code{data.frame} with variable
#'   importance values for each wavelength for each training iteration. If
#'   \code{return.model} is also \code{TRUE}, returns list of three elements
#'   with trained model first, model performance second, and variable importance
#'   last. Dimensions are \code{nrow = num.iterations}, \code{ncol =
#'   length(wavelengths)}. \item If \code{FALSE}, no variable importance is
#'   returned. Default is \code{FALSE}. }
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
#' @return \code{data.frame} with model performance statistics either in summary
#'   format (2 rows, one with mean and one with standard deviation of all
#'   training iterations) or in long format (number of rows =
#'   \code{num.iterations}). Also returns trained model if \code{return.model}
#'   is \code{TRUE}. If \code{FALSE}, returns results \code{data.frame} without
#'   model. Default is \code{FALSE}.
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
#'}
#'
#' @importFrom caret createDataPartition trainControl train
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
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::filter(study.name == "C16Mcal") %>%
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::select(sample.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   TrainSpectralModel(df = .,
#'                      tune.length = 3,
#'                      num.iterations = 3,
#'                      output.summary = TRUE,
#'                      return.model = FALSE,
#'                      best.model.metric = "RMSE",
#'                      stratified.sampling = TRUE)
TrainSpectralModel <- function(df,
                               num.iterations,
                               test.data = NULL,
                               tune.length = 50,
                               model.method = "pls",
                               output.summary = TRUE,
                               return.model = FALSE,
                               best.model.metric = "RMSE",
                               rf.variable.importance = FALSE,
                               stratified.sampling = TRUE,
                               cv.scheme = NULL,
                               trial1 = NULL,
                               trial2 = NULL,
                               trial3 = NULL,
                               split.test = FALSE,
                               verbose = TRUE) {
  # Error handling
  if(!(best.model.metric %in% c("RMSE", "Rsquared"))){
    stop('best.model.metric must be either "RMSE" or "Rsquared"')
  }

  if(!(model.method %in% c("pls", "rf", "svmLinear", "svmRadial"))){
    stop('model.method must be "pls", "rf", "svmLinear", or "svmRadial"')
  }

  if(rf.variable.importance & model.method != "rf"){
    stop('model.method must be "rf" if rf.variable.importance is TRUE')
  }

  if(!("reference" %in% colnames(df))){
    stop('The training dataset must include a column named "reference"')
  }

  if(!is.null(test.data) & !("reference" %in% colnames(test.data))){
    stop('The test dataset must include a column named "reference"')
  }

  if(!is.null(cv.scheme)){
    if(!(cv.scheme %in% c("CV1", "CV2", "CV0", "CV00"))){
      stop('cv.scheme must be NULL, "CV0", "CV00", "CV1", or "CV2"')
    }

    # Set num.iterations based on cv.scheme
    if(cv.scheme == "CV0" | cv.scheme == "CV00"){
      num.iterations <- 1
    } # else use provided number of iterations
  }

  # Create empty df to hold results
  df.colnames <- c("RMSEp", "R2p", "RPD", "RPIQ", "CCC", "Bias", "SEP",
                   "RMSEcv", "R2cv", "R2sp")
  if(model.method == "pls"){
    # Add extra column for best number of components
    df.colnames <- append(df.colnames, "best.ncomp")
  } else if(model.method == "rf"){
    # Add two extra columns for best ntree and best mtry
    df.colnames <- append(df.colnames, c("best.ntree", "best.mtry"))
    if(rf.variable.importance){
      rf.importance.df <- as.data.frame(matrix(data = NA,
                                               nrow = num.iterations,
                                               ncol = (ncol(df)-1)))
      colnames(rf.importance.df) <- colnames(df)[2:ncol(df)] # only wavelength columns
    }
  }
  results.df <- as.data.frame(matrix(data = NA, nrow = num.iterations,
                                     ncol = length(df.colnames)))
  colnames(results.df) <- df.colnames

  # Train model
  for (i in 1:num.iterations) {
    # set seed, different for each iteration for random samples
    set.seed(i)
    if(is.null(cv.scheme)){
      if(is.null(test.data)){
        if(stratified.sampling){
          # No separate test set provided
          # Stratified sampling to get representative sample of ground truth (reference column) values
          train.index <- unlist(caret::createDataPartition(df$reference, p=0.7))
        } else if(!stratified.sampling){
          # Random sample option (!stratified.sampling)
          train.index <- sort(sample(x = seq(from = 1, to = nrow(df), by = 1),
                                     size = 0.7 * nrow(df),
                                     replace = FALSE, prob = NULL))
        }
        # Whether stratified or random sampling, use the train.index to select training and test sets
        data.train <- df[train.index, ]
        data.test <- df[-train.index, ]
      } else if(!is.null(test.data)){
        # If fixed training and test sets provided
        if(split.test){
          # If split.test = T
          # Fixed training set + add 70% of samples from test set pool to training set
          train.index <- unlist(caret::createDataPartition(test.data$reference,
                                                           p = 0.7))
          data.train <- rbind(df, test.data[train.index, ])
          data.test <- test.data[-train.index, ]
        } else if(!split.test){
          # If fixed training and test sets provided but split.test = F
          data.train <- df
          data.test <- test.data
        }
      }
    } else if(!is.null(cv.scheme)){
      # cv.scheme present
      # Use selected cross-validation scheme
      formatted.lists <- FormatCV(trial1 = trial1,
                                  trial2 = trial2,
                                  trial3 = trial3,
                                  cv.scheme = cv.scheme,
                                  seed = i,
                                  remove.genotype = TRUE)
      data.train <- formatted.lists[[1]]
      data.test <- formatted.lists[[2]]
    }

    train.ref.spectra <- data.train %>% dplyr::select(.data$reference,
                                                      starts_with("X"))
    test.spectra <- data.test %>% dplyr::select(starts_with("X")) # exclude reference column from predictions
    if(num.iterations > 9){
      cv.seeds <- c(1:num.iterations)
    } else{
      cv.seeds <- c(1:10)
    }

    # Train hyperparameters with training data "ncomps" with training data
    #     Example// for 'pls', train hyperparameter "ncomps", where tune.length is number of ncomps tried
    if(model.method != "rf"){
      # 5-fold cross validation on training set
      cv.5 <- caret::trainControl(method = "cv", number = 5,
                                  savePredictions = TRUE, seeds = cv.seeds)
      data.trained <- caret::train(reference ~ ., data = train.ref.spectra,
                                   method = model.method,
                                   tuneLength = tune.length, trControl = cv.5,
                                   metric = best.model.metric)
    }

    if(model.method == "pls"){
      # Extract best number of components
      best.hyper <- data.trained$bestTune$ncomp
      # Put results as row in data frame
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra), # exclude reference column
                                             ncomp = best.hyper))
      R2cv <- pls::R2(data.trained$finalModel, ncomp = best.hyper)[["val"]][2]
      RMSEcv <- pls::RMSEP(data.trained$finalModel,
                           ncomp = best.hyper)[["val"]][2]

    } else if(model.method == "svmLinear"){
      best.hyper <- NA
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra)))
      R2cv <- NA
      RMSEcv <- NA

    } else if(model.method == "svmRadial"){
      best.hyper <- NA
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra)))
      R2cv <- NA
      RMSEcv <- NA

    } else if(model.method == "rf"){
      cv.oob <- caret::trainControl(method = "oob", number = 5,
                                    savePredictions = TRUE,
                                    seeds = list(cv.seeds, cv.seeds))
      data.trained <- caret::train(reference ~ ., data = train.ref.spectra,
                                   method = model.method,
                                   tuneLength = tune.length, trControl = cv.oob,
                                   metric = best.model.metric,
                                   importance = TRUE)
      best.hyper <- t(as.data.frame(c(data.trained$finalModel$ntree,
                                      data.trained$finalModel$mtry)))
      colnames(best.hyper) = c("ntree", "mtry")
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra),
                                             ntree = best.hyper[1],
                                             mtry = best.hyper[2]))
      R2cv <- NA
      RMSEcv <- NA
      if(rf.variable.importance){
        rf.importance.df[i,] <- t(importance(data.trained$finalModel, type=1))
      }
    }
    # Get model performance statistics
    reference.values <- data.test$reference

    R2sp <- cor(predicted.values, reference.values, method = "spearman") # Spearman's rank correlation
    results.i <- cbind(t(spectacles::postResampleSpectro(predicted.values,
                                                         reference.values)),
                       RMSEcv, R2cv, R2sp)

    if(model.method != "svmLinear"){
      results.df[i,] <- cbind(results.i, best.hyper)
    } else{
      results.df[i,] <- results.i
    }
  } # end loop

  #' @name getmode
  #' @description Get the mode of a set of numbers. Used in getting summary of results
  #' within [TrainSpectralModel()]
  #' @param vector.input The mode of this vector of numbers will be calculated by this function
  #' @return mode of the numbers in `vector.input`
  #' @export
  #' @keywords internal
  getmode <- function(vector.input){
    as.matrix(vector.input)
    unique.vector <- unique(vector.input)
    return(unique.vector[which.max(tabulate(match(vector.input,unique.vector)))])
  }

  # create and print summary data frame
  summary.df <- rbind(summarize_all(results.df, .funs = mean),
                      summarize_all(results.df, sd, na.rm = TRUE))
  summary.df$Summary_type = c("mean", "sd")
  summary.df %<>% dplyr::select(.data$Summary_type, .data$RMSEp:.data$R2sp)
  if(model.method == "pls"){
    # Report the mode of number of components, no standard deviation
    # This keeps the number of components as an integer and represents the value chosen most often
    summary.df$best.ncomp <- c(getmode(results.df$best.ncomp), NA)
  } else if(model.method == "rf"){
    # Report the mode of hyperparameters, no standard deviation
    summary.df$best.ntree <- c(getmode(results.df$best.ntree), NA)
    summary.df$best.mtry <- c(getmode(results.df$best.mtry), NA)
  }

  results.df$Iteration = 1:nrow(results.df)
  results.df %<>%
    dplyr::select(.data$Iteration, everything())

  if(return.model){
    # If model desired as output (return.model is TRUE), return list of c(trained model, results).
    # Create model with all input data (not just 70%). Results will give an idea of this model's performance,
    #     but they will have been generated with only subsets of the data.
    if (verbose) cat("\nReturning model...\n")
    if(model.method == "pls"){
      # Format df for plsr() function
      df.plsr <- as.data.frame(df[, 1:2])
      df.plsr$spectra <- as.matrix(df[3:ncol(df)])
      full.model <- pls::plsr(reference ~ spectra, ncomp = tune.length,
                              data = df.plsr)
    }
    if(model.method == "rf"){
      df.rf <- df %>% dplyr::select(.data$reference, starts_with("X"))
      full.model <- randomForest::randomForest(reference ~ ., data = df.rf,
                                               importance = FALSE,
                                  ntree = tune.length)
    }
    if(model.method == "svmLinear" | model.method == "svmRadial"){
      full.model <- caret::train(reference ~ ., data = df,
                                 method = model.method,
                          tuneLength = tune.length, trControl = cv.5,
                          metric = best.model.metric)
    }

    ifelse(output.summary,return(list(full.model, summary.df)),
           return(list(full.model, results.df)))
  } else{ # !(return.model)
    if(rf.variable.importance){
      ifelse(output.summary,
             return(list(summary.df, rf.importance.df)),
             return(list(results.df, rf.importance.df)))
    }
    # If model and variable importance not desired as output, return results only
    # (either in summary or in full format)
    # Output has 11 columns if model.method is "pls" or "svmRadial", 12 if "rf", and 10 if "svmLinear"
    ifelse(output.summary, return(summary.df), return(results.df))
  }

}
