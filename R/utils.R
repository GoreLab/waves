#' @title Internal utility functions
#' @name get_mode
#' @description Get the mode of a set of numbers. Used in getting summary of
#' results within [train_spectra()]
#'
#' @param vector.input The mode of this vector of numbers will be calculated
#' by this function
#' @return mode of the numbers in `vector.input`
#' @keywords internal
get_mode <- function(vector.input) {
  as.matrix(vector.input)
  unique.vector <- unique(vector.input)
  return(unique.vector[which.max(tabulate(match(vector.input,
                                                unique.vector)))])
}

#' @title Handle deprecation warnings for spectral functions
#' @name handle_deprecations
#' @description Internal function to handle deprecation warnings for parameters used across spectral functions
#' @param function_name Name of the calling function (e.g., "test_spectra", "train_spectra")
#' @param wavelengths,preprocessing,output.summary,rf.variable.importance,save.model,return.model Deprecated parameters
#' @return NULL (warnings are issued as side effects)
#' @keywords internal
#' @importFrom lifecycle is_present deprecate_warn
handle_deprecations <- function(function_name,
                                wavelengths = NULL,
                                preprocessing = NULL,
                                output.summary = NULL,
                                rf.variable.importance = NULL,
                                save.model = NULL,
                                return.model = NULL) {
  
  if (lifecycle::is_present(wavelengths)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(function_name, "(wavelengths)"),
      details = "Wavelength specification is now inferred from column names."
    )
  }

  if (lifecycle::is_present(preprocessing)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(function_name, "(preprocessing)"),
      details = "To test all pretreatment methods, use 'pretreatment = 1:13'.
      To test only raw data, use 'pretreatment = 1'."
    )
  }

  if (lifecycle::is_present(rf.variable.importance)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(function_name, "(rf.variable.importance)"),
      details = "Variable importance is now output by default when
      `model.method` is set to `pls` or `rf`."
    )
  }

  if (lifecycle::is_present(output.summary)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(function_name, "(output.summary)"),
      details = "Summary is now default output alongside full results."
    )
  }

  if (lifecycle::is_present(save.model)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(function_name, "(save.model)"),
      details = "Models are now saved by default."
    )
  }

  if (lifecycle::is_present(return.model)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = paste0(function_name, "(return.model)"),
      details = "Trained models are now default output alongside full results."
    )
  }
}

#' @title Validate spectral function inputs
#' @name validate_inputs
#' @description Internal function to validate common inputs for spectral functions
#' @param train.data,test.data,cv.scheme,trial1,trial2,trial3,model.method,tune.length,best.model.metric,proportion.train Input parameters to validate
#' @return NULL (errors are thrown as side effects if validation fails)
#' @keywords internal
#' @importFrom rlang abort has_name
validate_inputs <- function(train.data,
                            test.data = NULL,
                            cv.scheme = NULL,
                            trial1 = NULL,
                            trial2 = NULL,
                            trial3 = NULL,
                            model.method = "pls",
                            tune.length = 50,
                            best.model.metric = "RMSE",
                            proportion.train = 0.7) {
  
  # Model method validation
  if (!(best.model.metric %in% c("RMSE", "Rsquared"))) {
    rlang::abort('best.model.metric must be either "RMSE" or "Rsquared"')
  }

  if (!(model.method %in% c("pls", "rf", "svmLinear", "svmRadial"))) {
    rlang::abort('model.method must be "pls", "rf", "svmLinear", or "svmRadial"')
  }

  # Data column validation
  if (!rlang::has_name(train.data, "reference")) {
    rlang::abort('The training dataset must include a column named "reference"')
  }

  if (!is.null(test.data) && !(rlang::has_name(test.data, "reference"))) {
    rlang::abort('The test dataset must include a column named "reference"')
  }

  if (!(rlang::has_name(train.data, "unique.id"))) {
    rlang::abort('The training dataset must include a column named "unique.id"')
  }

  if (!is.null(test.data) && !(rlang::has_name(test.data, "unique.id"))) {
    rlang::abort('The test dataset must include a column named "unique.id"')
  }

  # CV scheme validation
  if (!is.null(cv.scheme)) {
    if (!(cv.scheme %in% c("CV1", "CV2", "CV0", "CV00"))) {
      rlang::abort('cv.scheme must be NULL, "CV0", "CV00", "CV1", or "CV2"')
    }
    if (is.null(trial1)) {
      rlang::abort("trial1 must be provided if using cv.scheme")
    }
    if (is.null(trial2)) {
      rlang::abort("trial2 must be provided if using cv.scheme")
    }
    if (sum(colnames(trial1) != colnames(trial2)) > 0) {
      rlang::abort("Column names must match for trial1 and trial2
                   if using cv.scheme")
    }
    if (!is.null(trial3) && sum(colnames(trial1) != colnames(trial3)) > 0) {
      rlang::abort("Column names must match for trial1, trial2, and trial3
                   if using cv.scheme and including trial3")
    }
  }

  # Missing values validation
  if (nrow(train.data) != nrow(na.omit(train.data))) {
    rlang::abort("Training data cannot contain missing values.")
  }

  if (!is.null(test.data) && (nrow(test.data) != nrow(na.omit(test.data)))) {
    rlang::abort("Test data cannot contain missing values.
                 Either omit missing values or exclude training data
                 (set as NULL).")
  }

  # Parameter range validation
  if (proportion.train > 1 || proportion.train < 0) {
    rlang::abort("'proportion.train' must be a number between 0 and 1")
  }

  # Model method specific validation
  if (model.method == "rf" && tune.length > 5) {
    rlang::abort("The waves implementation of the random forest algorithm uses
                 oob cross-validation for model training
                 and requires a tune length of 5.")
  }
}

#' @title Partition data for training and testing
#' @name partition_data
#' @description Internal function to handle data partitioning for train_spectra
#' @param df Training data frame
#' @param test.data Test data frame (optional)
#' @param iteration Current iteration number
#' @param num.iterations Total number of iterations
#' @param stratified.sampling Whether to use stratified sampling
#' @param proportion.train Proportion of data to use for training
#' @param split.test Whether to split test data
#' @param cv.scheme Cross-validation scheme
#' @param trial1,trial2,trial3 Trial data for CV scheme
#' @param train.index Pre-calculated training indices (for stratified sampling)
#' @return List containing train and test data for current iteration
#' @keywords internal
#' @importFrom caret createDataPartition
partition_data <- function(df,
                                     test.data = NULL,
                                     iteration,
                                     num.iterations,
                                     stratified.sampling = TRUE,
                                     proportion.train = 0.7,
                                     split.test = FALSE,
                                     cv.scheme = NULL,
                                     trial1 = NULL,
                                     trial2 = NULL,
                                     trial3 = NULL,
                                     train.index = NULL) {
  
  # Determine partition input
  if (is.null(test.data)) {
    partition.input.df <- df
  } else {
    partition.input.df <- test.data
  }
  
  partition.nrow <- nrow(partition.input.df)
  train.size <- round(proportion.train * partition.nrow)
  
  # Handle fixed training and test sets
  if (!is.null(test.data) && !split.test) {
    return(list(train = df, test = test.data))
  }
  
  # Handle CV scheme
  if (!is.null(cv.scheme)) {
    formatted.lists <- format_cv(
      trial1 = trial1,
      trial2 = trial2,
      trial3 = trial3,
      cv.scheme = cv.scheme,
      stratified.sampling = stratified.sampling,
      proportion.train = proportion.train,
      seed = iteration,
      remove.genotype = TRUE
    )
    return(list(train = formatted.lists$train.set, test = formatted.lists$test.set))
  }
  
  # Handle stratified sampling
  if (stratified.sampling) {
    if (is.null(test.data)) {
      # No test set provided
      data.train <- df[train.index[[iteration]], ]
      data.test <- df[-train.index[[iteration]], ]
    } else if (split.test) {
      # Test set provided and split in a stratified random manner
      data.train <- rbind(df, test.data[train.index[[iteration]], ])
      data.test <- test.data[-train.index[[iteration]], ]
    }
  } else {
    # Random sample (not stratified)
    set.seed(iteration)  # Different seed for each iteration
    train.idx <- sort(sample(
      x = seq_len(partition.nrow),
      size = train.size,
      replace = FALSE, prob = NULL
    ))
    
    if (is.null(test.data)) {
      # No test set provided
      data.train <- df[train.idx, ]
      data.test <- df[-train.idx, ]
    } else if (split.test) {
      # Test set provided and split randomly
      data.train <- rbind(df, test.data[train.idx, ])
      data.test <- test.data[-train.idx, ]
    }
  }
  
  return(list(train = data.train, test = data.test))
}

#' @title Create cross-validation training control
#' @name create_cv_control
#' @description Internal function to create consistent caret trainControl objects
#' @param k.folds Number of cross-validation folds
#' @param cv.seeds Seeds for cross-validation
#' @return caret trainControl object
#' @keywords internal
#' @importFrom caret trainControl
create_cv_control <- function(k.folds = 5, cv.seeds) {
  caret::trainControl(
    method = "repeatedcv",
    number = k.folds,
    savePredictions = TRUE,
    seeds = cv.seeds
  )
}

#' @title Train individual spectral model
#' @name train_individual_model
#' @description Internal function to train a single model iteration
#' @param train.ref.spectra Training data with reference and spectral columns
#' @param test.spectra Test spectral data (matrix)
#' @param model.method Model algorithm to use
#' @param tune.length Tuning parameter
#' @param k.folds Number of cross-validation folds
#' @param best.model.metric Metric for model selection
#' @param cv.seeds Seeds for cross-validation
#' @return List containing predictions and model performance metrics
#' @keywords internal
#' @importFrom caret trainControl train varImp
#' @importFrom pls R2 RMSEP
#' @importFrom stats predict
train_individual_model <- function(train.ref.spectra,
                                   test.spectra,
                                   model.method = "pls",
                                   tune.length = 50,
                                   k.folds = 5,
                                   best.model.metric = "RMSE",
                                   cv.seeds) {
  
  # Initialize return values
  predicted.values <- NULL
  best.ncomp <- NA
  best.ntree <- NA
  best.mtry <- NA
  R2cv <- NA
  RMSEcv <- NA
  
  if (model.method != "rf") {
    # 5-fold cross validation on training set
    cv.kfold <- create_cv_control(k.folds = k.folds, cv.seeds = cv.seeds)

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
    predicted.values <- as.numeric(predict(data.trained$finalModel,
      newdata = test.spectra,
      ncomp = best.ncomp
    ))
    R2cv <- pls::R2(data.trained$finalModel, ncomp = best.ncomp)[["val"]][2]
    RMSEcv <- pls::RMSEP(data.trained$finalModel, ncomp = best.ncomp)[["val"]][2]
    
  } else if (model.method == "svmLinear") {
    predicted.values <- as.numeric(predict(data.trained, newdata = test.spectra))
    
  } else if (model.method == "svmRadial") {
    predicted.values <- as.numeric(predict(data.trained, newdata = test.spectra))
    
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
    best.ntree <- data.trained$finalModel$ntree
    best.mtry <- data.trained$finalModel$mtry
    predicted.values <- as.numeric(predict(data.trained$finalModel,
      newdata = test.spectra,
      ntree = best.ntree,
      mtry = best.mtry
    ))
  }

  # Variable importance (for pls and rf only)
  importance.df <- NULL
  if (model.method %in% c("pls", "rf")) {
    importance.df <- caret::varImp(data.trained$finalModel)
  }

  return(list(
    predictions = predicted.values,
    best.ncomp = best.ncomp,
    best.ntree = best.ntree,
    best.mtry = best.mtry,
    R2cv = R2cv,
    RMSEcv = RMSEcv,
    importance = importance.df
  ))
}

#' @title Calculate model performance statistics
#' @name calculate_performance
#' @description Internal function to calculate performance statistics for model predictions
#' @param predicted.values Vector of predicted values
#' @param reference.values Vector of reference (observed) values
#' @param iteration Current iteration number
#' @param model.method Model algorithm used
#' @param R2cv,RMSEcv Cross-validation metrics (if available)
#' @param best.ncomp,best.ntree,best.mtry Best tuning parameters
#' @param importance.df Variable importance data frame
#' @return List containing performance results, predictions, and importance
#' @keywords internal
#' @importFrom spectacles postResampleSpectro
#' @importFrom stats cor
#' @importFrom tibble rownames_to_column
calculate_performance <- function(predicted.values,
                                        reference.values,
                                        iteration,
                                        model.method,
                                        R2cv = NA,
                                        RMSEcv = NA,
                                        best.ncomp = NA,
                                        best.ntree = NA,
                                        best.mtry = NA,
                                        importance.df = NULL,
                                        unique.ids = NULL) {
  
  # Calculate correlation statistics
  R2sp <- cor(predicted.values, reference.values, method = "spearman")^2
  
  # Call spectacles with proper error handling for different return formats
  tryCatch({
    spectacles.result <- spectacles::postResampleSpectro(
      pred = predicted.values,
      obs = reference.values
    )
    spectacles.df <- as.data.frame(t(spectacles.result))
  }, error = function(e) {
    # If spectacles fails or returns unexpected format, create default values
    spectacles.df <<- data.frame(
      RMSE = NA, Rsquared = NA, RPD = NA, RPIQ = NA, 
      CCC = NA, Bias = NA, SEP = NA
    )
  })
  
  # Ensure we have a valid data frame with the expected columns
  expected.cols <- c("RMSE", "Rsquared", "RPD", "RPIQ", "CCC", "Bias", "SEP")
  if (!all(expected.cols %in% names(spectacles.df))) {
    spectacles.df <- data.frame(
      RMSE = NA, Rsquared = NA, RPD = NA, RPIQ = NA, 
      CCC = NA, Bias = NA, SEP = NA
    )
  }
  
  # Compile results
  results.df <- data.frame(
    Iteration = iteration,
    RMSEp = spectacles.df$RMSE,
    R2p = spectacles.df$Rsquared,
    RPD = spectacles.df$RPD,
    RPIQ = spectacles.df$RPIQ,
    CCC = spectacles.df$CCC,
    Bias = spectacles.df$Bias,
    SEP = spectacles.df$SEP,
    RMSEcv = RMSEcv,
    R2cv = R2cv,
    R2sp = R2sp,
    best.ncomp = best.ncomp,
    best.ntree = best.ntree,
    best.mtry = best.mtry,
    stringsAsFactors = FALSE
  )
  
  # Compile predictions
  predictions.df <- data.frame(
    Iteration = iteration,
    ModelType = model.method,
    unique.id = unique.ids,
    reference = reference.values,
    predicted = predicted.values,
    stringsAsFactors = FALSE
  )
  
  # Format importance data if available
  formatted.importance <- NULL
  if (!is.null(importance.df)) {
    formatted.importance <- cbind(
      "Iteration" = iteration, 
      "ModelType" = model.method,
      importance.df
    ) %>%
      tibble::rownames_to_column(var = "wavelength")
    rownames(formatted.importance) <- NULL
  }
  
  return(list(
    results = results.df,
    predictions = predictions.df,
    importance = formatted.importance
  ))
}

#' @title Process pretreatment data for test_spectra
#' @name process_pretreatment_data
#' @description Internal function to handle pretreatment data processing and extraction
#' @param df.list List of pretreated data frames
#' @param methods.list List of method names
#' @param i Current pretreatment index
#' @param n.train Number of training observations
#' @param n.test Number of test observations
#' @param cv.scheme CV scheme (if any)
#' @param trial1,trial2,trial3 Trial data frames (if using cv.scheme)
#' @return List containing processed training and test data
#' @keywords internal
process_pretreatment_data <- function(df.list,
                                       methods.list,
                                       i,
                                       n.train,
                                       n.test,
                                       cv.scheme = NULL,
                                       trial1 = NULL,
                                       trial2 = NULL,
                                       trial3 = NULL) {
  
  # Extract preprocessed data
  processed.train.data <- df.list[[methods.list[i]]][1:n.train, ]
  
  if (n.test == 0) {
    processed.test.data <- NULL
  } else {
    processed.test.data <- df.list[[methods.list[i]]][(n.train + 1):(n.train + n.test), ]
  }

  # Handle CV scheme data if present
  if (!is.null(cv.scheme)) {
    processed.trial1 <- df.list[[methods.list[i]]][seq_len(nrow(trial1)), ]
    processed.trial2 <- df.list[[methods.list[i]]][(nrow(trial1) + 1):(nrow(trial1) + nrow(trial2)), ]
    processed.trial3 <- df.list[[methods.list[i]]][(nrow(trial1) + nrow(trial2) + 1):nrow(df.list[[methods.list[i]]]), ]
  } else {
    processed.trial1 <- NULL
    processed.trial2 <- NULL
    processed.trial3 <- NULL
  }

  return(list(
    train.data = processed.train.data,
    test.data = processed.test.data,
    trial1 = processed.trial1,
    trial2 = processed.trial2,
    trial3 = processed.trial3
  ))
}

#' @title Aggregate results for multiple pretreatments
#' @name aggregate_pretreatment_results
#' @description Internal function to combine results from multiple pretreatments
#' @param training.results.i Current training results
#' @param methods.list List of method names
#' @param i Current pretreatment index
#' @param pretreatment Vector of pretreatment indices
#' @param counter Current counter value
#' @param existing.results Existing aggregated results (if any)
#' @return List of aggregated results
#' @keywords internal
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magrittr %>%
aggregate_pretreatment_results <- function(training.results.i,
                                            methods.list,
                                            i,
                                            pretreatment,
                                            counter,
                                            existing.results = NULL) {
  
  # Process summary based on number of pretreatments
  if (length(pretreatment) != 1) {
    # Add Pretreatment column to each data.frame in the training results list
    for (j in 2:length(training.results.i)) {
      training.results.i[[j]] <- cbind("Pretreatment" = methods.list[i],
                                       training.results.i[[j]])
      rownames(training.results.i[[j]]) <- NULL
    }

    # Reformat summary statistics data.frame for multiple pretreatments
    summary.i <- training.results.i$summary.model.performance %>%
      tidyr::pivot_longer(cols = .data$RMSEp:.data$best.mtry) %>%
      tidyr::pivot_wider(
        id_cols = c(.data$Pretreatment),
        names_from = c(.data$name, .data$SummaryType),
        names_sep = "_"
      )
  } else {
    summary.i <- training.results.i$summary.model.performance
  }

  # Initialize or update results
  if (counter == 1) {
    # Set up results compilations in first iteration
    if (length(pretreatment) != 1) {
      model.list <- list(training.results.i$model)
    } else {
      model.list <- training.results.i$model
    }
    summary.df <- summary.i
    results.df <- training.results.i$model.performance
    predictions.df <- training.results.i$predictions
    importance.df <- training.results.i$importance
  } else {
    # Add new results to existing objects
    model.list <- append(existing.results$model.list, list(training.results.i$model))
    summary.df <- rbind(existing.results$summary.df, summary.i)
    results.df <- rbind(existing.results$results.df, training.results.i$model.performance)
    predictions.df <- rbind(existing.results$predictions.df, training.results.i$predictions)
    importance.df <- rbind(existing.results$importance.df, training.results.i$importance)
  }

  return(list(
    model.list = model.list,
    summary.df = summary.df,
    results.df = results.df,
    predictions.df = predictions.df,
    importance.df = importance.df
  ))
}
