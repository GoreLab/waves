#' @title Save spectral prediction model and model performance statistics
#' @name SaveModel
#' @description Saves model and model statistics to model.save.folder as model.name.Rds
#' and model.name_stats.csv respectively
#' @details Wrapper that uses [DoPreprocessing()], [FormatCV()] and [TrainSpectralModel()] functions.
#' @author Jenna Hershberger
#'
#' @param df a `data.frame` object. First column contains unique identifier, second
#' contains reference values, followed by spectral columns to the right. No missing
#' values permitted! Include no other columns to right of spectra!
#' @param preprocessing If `TRUE`, function will evaluate the full panel of preprocessing
#' techniques and determine the best using the best.model.metric. If `FALSE`, no
#' preprocessing will be applied and raw data will be used. Default is `FALSE`.
#' @param save.model If `TRUE`, the trained model will be saved in .Rds format to the
#' location specified by `model.save.folder`. If `FALSE`, model will be output by function
#' but will not save to file. Default is `TRUE`.
#' @param model.save.folder Path to folder where model will be saved. If not provided,
#' will save to working directory.
#' @param model.name Name that model will be saved as in `model.save.folder`. Default is 'PredictionModel'.
#' @param best.model.metric Metric used to decide which model is best. Must be either "RMSE or "Rsquared".
#' @param tune.length Sets the sample space for tuning of hyperparameters.
#' (Example// "ncomp", or number of components for "pls" model; "ntree" and "mtry" for "rf" model)
#' @param model.method Model type to use for training. Valid options include:
#' *"pls": Partial least squares regression (Default)
#' *"rf": Random forest
#' *"svmLinear": Support vector machine with linear kernel
#' *"svmRadial": Support vector machine with radial kernel
#' @param num.iterations Number of training iterations to perform for model statistics
#' @param wavelengths List of wavelengths represented by each column in `train.data`
#' @param stratified.sampling boolean that, if `TRUE``, will trigger the use of stratified random
#' sampling for selection of training and test sets.This term is only used if separate test set is not
#' provided. Default is `TRUE`.
#' @param trial1 `data.frame` object that is for use only when `cv.scheme` is provided.
#' Contains the trial to be tested in subsequent model training functions. The first column
#' contains unique identifiers, second contains genotypes, third contains reference values,
#' followed by spectral columns. Include no other columns to right of spectra! Column names
#' of spectra must start with "X", reference column must be named "reference", and genotype column
#' must be named "genotype".
#' @param trial2 `data.frame` object that is for use only when `cv.scheme` is provided.
#' This data.frame contains a trial that has overlapping genotypes with `trial1`
#' but that were grown in a different site/year (different environment). Formatting must be
#' consistent with `trial1`.
#' @param trial3 `data.frame` object that is for use only when `cv.scheme` is provided.
#' This data.frame contains a trial that may or may not contain genotypes that overlap with `trial1`.
#' Formatting must be consistent with `trial1`.
#' @param cv.scheme A cross validation (CV) scheme from Jarquín et al., 2017.
#' Options for cv.scheme include:
#' *"CV1", untested lines in tested environments
#' *"CV2", tested lines in tested environments
#' *"CV0", tested lines in untested environments
#' *"CV00", untested lines in untested environments
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @importFrom tidyselect everything
#' @importFrom rlang .data
#' @importFrom utils write.csv
#'
#' @return List of model stats (in `data.frame`) and trained model object. Saves both to
#' `model.save.folder` as well. To use optimally trained model for predictions, extract `$finalModel`
#' and use tuned parameters from `$bestTune`
#' @export
#'
#' @examples
SaveModel <- function(df,
                      preprocessing = F,
                      save.model = T,
                      model.save.folder = NULL,
                      model.name = "PredictionModel", # TODO automatically generate a name based on sys date
                      best.model.metric = "RMSE",
                      tune.length = 50,
                      model.method = "pls",
                      num.iterations = 10,
                      wavelengths = 740:1070,
                      stratified.sampling = T,
                      cv.scheme = NULL,
                      trial1 = NULL,
                      trial2 = NULL,
                      trial3 = NULL
) {
  # Error handling
  if(!(best.model.metric %in% c("RMSE", "Rsquared"))){
    stop('best.model.metric must be either "RMSE" or "Rsquared"')
  }

  if(length(wavelengths) != ncol(df) - 2) {
    stop("Number of spectral columns in train.data (ncol(train.data) - 2) must match number of wavelengths")
  }

  if(nrow(df) != nrow(na.omit(df))) {
    stop("Training data cannot contain missing values.")
  }

  if(!is.character(model.name)){
    stop("model.name must be a string!")
  }

  if(is.null(model.save.folder)){
    model.save.folder = getwd()
  }

  # Choose best preprocessing method and set up training set
  methods.list <- c("Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG", "SNVSG", "SGD1", "SG.D1W5",
                    "SG.D1W11", "SG.D2W5", "SG.D2W11")

  if (!preprocessing) {
    cat("Training model...\n")
    formatted.df <- DoPreprocessing(df = df, test.data = NULL,
                                    preprocessing.method = 1,
                                    wavelengths = wavelengths)
    training.results <- TrainSpectralModel(df = formatted.df, num.iterations = num.iterations,
                                           test.data = NULL,tune.length = tune.length,
                                           model.method = model.method, output.summary = TRUE,
                                           return.model = TRUE, stratified.sampling = stratified.sampling,
                                           best.model.metric = best.model.metric, cv.scheme = cv.scheme,
                                           trial1 = trial1, trial2 = trial2, trial3 = trial3)
    best.model <- training.results[[1]]
    best.model.stats <- training.results[[2]]
    best.model.stats <- best.model.stats %>%
      dplyr::mutate(Pretreatment = "Raw_data") %>% # Create "Pretreatment" column
      dplyr::select(.data$Pretreatment, everything()) # move "Pretreatment" column to far left
    print(best.model.stats)
  } else {
    cat("Preprocessing...\n")
    # Preprocess data and use model statistics from training function to determine best model
    df.list <- DoPreprocessing(df = df, test.data = NULL,
                               preprocessing.method = 1:13,
                               wavelengths = wavelengths)

    # Set up empty list and data frame for training output
    model.list <- list()
    results.df <- as.data.frame(matrix(nrow = 13, ncol = 17))
    results.df[,1] <- methods.list

    for(i in 1:13){
      cat(paste0("Training model ", i, " of 13: (", methods.list[i], ").\n"))
      # Train model
      training.results.i <- TrainSpectralModel(df = df.list[[i]], num.iterations = num.iterations,
                                               test.data = NULL, tune.length = tune.length,
                                               model.method = model.method, output.summary = TRUE,
                                               return.model = TRUE, # Outputs list of c(model, model performance metrics)
                                               stratified.sampling = stratified.sampling,
                                               best.model.metric = best.model.metric,
                                               cv.scheme = cv.scheme,
                                               trial1 = trial1, trial2 = trial2, trial3 = trial3)
      model.list[[i]] <- training.results.i[[1]] # Extract model from training output
      # Format output
      # Put pretreatment name in first column followed by means and standard deviations for each statistic
      results.df$Pretreatment[i] <- methods.list[i]
      spectacle.results.i <- training.results.i[[2]] %>% dplyr::select(.data$RMSE:.data$Spearman)
      hyperparameter.results.i <- training.results.i[[2]] %>%
        dplyr::select(-(.data$Summary_type:.data$Spearman)) # works even if no hyperparameter columns
      results.df[i, 2:ncol(results.df)] <- data.frame(spectacle.results.i[1,], # row 1 is means
                                                      spectacle.results.i[2,], # row 2 is standard deviations
                                                      hyperparameter.results.i[1,]) # first row is values, second is just NA
    } # End loop of pretreatments

    colnames(results.df) <- c("Pretreatment", "RMSE", "Rsquared", "RPD", "RPIQ", "CCC", "Bias", "SE", "Spearman",
                              "RMSE.sd", "Rsquared.sd", "RPD.sd", "RPIQ.sd", "CCC.sd", "Bias.sd", "SE.sd", "Spearman.sd")
    cat("\nTraining Summary:\n")
    print(results.df)

    # Use results data frame to determine best preprocessing technique
    best.type.num <- ifelse(best.model.metric == "RMSE", which.min(results.df$RMSE),
                            which.max(results.df$Rsquared))
    cat(paste0("\nBest preprocessing technique: ", results.df$Pretreatment[best.type.num], "\n"))

    # Set chosen model as best.model for export
    best.model <- model.list[[best.type.num]]
    best.model.stats <- results.df[best.type.num,]

  } # End preprocessing if statement

  if(save.model){
    cat(paste0("\nSaving model and model statistics to ", model.save.folder, ".\n"))
    # output stats to model.save.folder as 'model.name_stats.csv'
    write.csv(best.model.stats,
              file = paste0(model.save.folder, '/', model.name, '_stats.csv'), row.names = F)
    # save model in save location as 'model.name.Rds'
    saveRDS(best.model, file = paste0(model.save.folder, '/', model.name, ".Rds"))
  }

  # output list of model stats data frame and model
  output.list <- list(best.model.stats, best.model)
  return(output.list)
}
