#' @title Train model based spectral data to predict reference values
#' @name TrainSpectralModel
#' @description #TODO
#' @details #TODO
#' @author Jenna Hershberger
#'
#' @inheritParams FormatCV
#' @param df `data.frame` object. First column contains unique identifiers, second contains
#' reference values, followed by spectral columns. Include no other columns to right of spectra!
#' Column names of spectra must start with "X" and reference column must be named "reference"
#' @param num.iterations Number of training iterations to perform
#' @param test.data `data.frame` with same specifications as `df`. Use if specific test set is
#' desired for hyperparameter tuning. If `NULL`, function will automatically train with a
#' stratified sample of 70\%. Default is `NULL`.
#' @param tune.length Number deliniating search space for tuning of the PLSR hyperparameter `ncomp`.
#' Default is 50.
#' @param model.method Model type to use for training. Valid options include:
#' *"pls": Partial least squares regression (Default)
#' *"rf": Random forest
#' *"svmLinear": Support vector machine with linear kernel
#' *"svmRadial": Support vector machine with radial kernel
#' @param output.summary boolean that controls function output.
#' *If `TRUE`, a summary df will be output (1st row = means, 2nd row = standard deviations). Default is `TRUE`.
#' *If `FALSE`, entire results data frame will be output
#' @param return.model boolean that, if `TRUE`, causes the function to return the trained model
#' in addition to the results data frame.
#' *If TRUE, function return list of c(model, results).
#' *If FALSE, returns results data frame without model. Default is FALSE.
#' @param best.model.metric Metric used to decide which model is best. Must be either "RMSE" or "Rsquared"
#' @param rf.variable.importance
#' *If `TRUE`, `model.method` must be set to "rf". Returns a list with a model performance
#' `data.frame` and a second `data.frame` with variable importance values for each wavelength
#' for each training iteration. If `return.model` is also `TRUE`, returns list of three elements
#' with trained model first, model performance second, and variable importance last. Dimensions are
#' `nrow = num.iterations`, `ncol = length(wavelengths)`.
#' *If `FALSE`, no variable importance is returned. Default is `FALSE`.
#' @param stratified.sampling If `TRUE`, training and test sets will be selected using stratified
#' random sampling. This term is only used if `test.data = NULL`. Default is `TRUE`.
#' @param split.test boolean that allows for a fixed training set and a split test set.
#' Example// train model on data from two breeding programs and a stratified subset (70\%)
#' of a third and test on the remaining samples (30\%)  of the third. If `FALSE`, the entire provided
#' test set `test.data` will remain as a testing set or if none is provided, 30\% of the provided
#' `train.data` will be used for testing. Default is `FALSE`.
#'
#' @return `data.frame` with model performance statistics (RMSE, Rsquared, RPD, RPIQ, CCC, Bias, SE, K)
#' either in summary format (2 rows, one with mean and one with standard deviation of all training iterations)
#' or in long format (number of rows = `num.iterations`).
#' Also returns trained model if `return.model` is `TRUE`.
#'
#' @importFrom caret createDataPartition trainControl train
#' @importFrom dplyr select mutate summarize_all
#' @importFrom tidyselect starts_with everything
#' @importFrom magrittr %>% %<>%
#' @importFrom stats cor predict sd
#' @importFrom spectacles postResampleSpectro
#' @importFrom randomForest importance
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
TrainSpectralModel <- function(df,
                               num.iterations,
                               test.data = NULL,
                               tune.length = 50,
                               model.method = "pls",
                               output.summary = TRUE,
                               return.model = F,
                               best.model.metric = "RMSE",
                               rf.variable.importance = F,
                               stratified.sampling = T,
                               cv.scheme = NULL,
                               trial1 = NULL,
                               trial2 = NULL,
                               trial3 = NULL,
                               split.test = F) {
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
      stop('cv.scheme must be "CV0", "CV00", "CV1", or "CV2"')
    }

    # Set num.iterations based on cv.scheme
    if(cv.scheme == "CV0" | cv.scheme == "CV00"){
      num.iterations <- 1
    } # else use provided number of iterations
  }

  # Create empty df to hold results
  df.colnames <- c("RMSE", "Rsquared", "RPD", "RPIQ", "CCC", "Bias", "SE", "Spearman")
  if(model.method == "pls"){
    # Add extra column for best number of components
    df.colnames <- append(df.colnames, "best.ncomp")
  } else if(model.method == "rf"){
    # Add two extra columns for best ntree and best mtry
    df.colnames <- append(df.colnames, c("best.ntree", "best.mtry"))
    if(rf.variable.importance){
      rf.importance.df <- as.data.frame(matrix(data = NA, nrow = num.iterations, ncol = (ncol(df)-1)))
      colnames(rf.importance.df) <- colnames(df)[2:ncol(df)] # only wavelength columns
    }
  }
  results.df <- as.data.frame(matrix(data = NA, nrow = num.iterations, ncol = length(df.colnames)))
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
                                     replace = F, prob = NULL))
        }
        # Whether stratified or random sampling, use the train.index to select training and test sets
        data.train <- df[train.index, ]
        data.test <- df[-train.index, ]
      } else if(!is.null(test.data)){
        # If fixed training and test sets provided
        if(split.test){
          # If split.test = T
          # Fixed training set + add 70% of samples from test set pool to training set
          train.index <- unlist(caret::createDataPartition(test.data$reference, p=0.7))
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

    train.ref.spectra <- data.train %>% dplyr::select(.data$reference, starts_with("X"))
    test.spectra <- data.test %>% dplyr::select(starts_with("X")) # exclude reference column from predictions

    # Train hyperparameters with training data "ncomps" with training data
    #     Example// for 'pls', train hyperparameter "ncomps", where tune.length is number of ncomps tried
    if(model.method != "rf"){
      # 5-fold cross validation on training set
      cv.5 <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
      data.trained <- train(reference ~ ., data = train.ref.spectra, method = model.method,
                            tuneLength = tune.length, trControl = cv.5, metric = best.model.metric)
    }

    if(model.method == "pls"){
      # Extract best number of components
      best.hyper <- data.trained$bestTune$ncomp
      # Put results as row in data frame
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra), # exclude reference column
                                             ncomp = best.hyper))

    } else if(model.method == "svmLinear"){
      best.hyper <- NA
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra)))

    } else if(model.method == "svmRadial"){
      best.hyper <- NA
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra)))

    } else if(model.method == "rf"){
      cv.oob <- trainControl(method = "oob", number = 5, savePredictions = TRUE)
      data.trained <- train(reference ~ ., data = train.ref.spectra, method = model.method,
                            tuneLength = tune.length, trControl = cv.5, metric = best.model.metric,
                            importance = TRUE)
      best.hyper <- t(as.data.frame(c(data.trained$finalModel$ntree, data.trained$finalModel$mtry)))
      colnames(best.hyper) = c("ntree", "mtry")
      predicted.values <- as.numeric(predict(data.trained$finalModel,
                                             newdata = as.matrix(test.spectra),
                                             ntree = best.hyper[1],
                                             mtry = best.hyper[2]))
      if(rf.variable.importance){
        rf.importance.df[i,] <- t(importance(data.trained$finalModel, type=1))
      }
    }
    # Get model performance statistics
    reference.values <- data.test$reference

    Spearman <- cor(predicted.values, reference.values, method = "spearman")
    results.i <- cbind(t(postResampleSpectro(predicted.values, reference.values)), Spearman)

    if(model.method == "pls" | model.method == "rf"){
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
  summary.df %<>%
    dplyr::select(.data$Summary_type, .data$RMSE:.data$Spearman)
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
    full.model <- train(reference ~ ., data = df, method = model.method,
                        tuneLength = tune.length, trControl = cv.5, metric = best.model.metric)
    if(output.summary){
      ifelse(rf.variable.importance,
             return(list(full.model, summary.df, rf.importance.df)),
             return(list(full.model, summary.df)))
    } else{
      ifelse(rf.variable.importance,
             return(list(full.model, results.df, rf.importance.df)),
             return(list(full.model, results.df)))
    }
  } else{
    if(rf.variable.importance){
      ifelse(output.summary,
             return(list(summary.df, rf.importance.df)),
             return(list(results.df, rf.importance.df)))
    }
    # If model and variable importance not desired as output, return results only
    # (either in summary or in full format)
    # Output has 9 columns if model.method is "pls" or "svmRadial", 10 if "rf", and 8 if "svmLinear"
    ifelse(output.summary, return(summary.df), return(results.df))
  }

}
