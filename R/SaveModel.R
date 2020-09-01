#' @title Save spectral prediction model and model performance statistics
#' @name SaveModel
#' @description Saves spectral prediction model and model statistics to
#'   \code{model.save.folder} as \code{model.name.Rds} and
#'   \code{model.name_stats.csv} respectively
#' @details Wrapper that uses \code{\link{DoPreprocessing}},
#'   \code{\link{FormatCV}}, and \code{\link{TrainSpectralModel}} functions.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams TestModelPerformance
#' @inheritParams TrainSpectralModel
#' @inheritParams DoPreprocessing
#' @param save.model If \code{TRUE}, the trained model will be saved in .Rds
#'   format to the location specified by \code{model.save.folder}. If
#'   \code{FALSE}, model will be output by function but will not save to file.
#'   Default is \code{TRUE}.
#' @param autoselect.preprocessing Boolean that, if \code{TRUE}, will choose the
#'   preprocessing method for the saved model using the
#'   \code{best.model.metric}. If \code{FALSE}, the user must supply the
#'   preprocessing method (1-12, see \code{\link{DoPreprocessing}()}
#'   documentation for more information) of the saved model. Default is
#'   \code{TRUE}.
#' @param model.save.folder Path to folder where model will be saved. If not
#'   provided, will save to working directory.
#' @param model.name Name that model will be saved as in
#'   \code{model.save.folder}. Default is "PredictionModel".
#' @param wavelengths List of wavelengths represented by each column in
#'   \code{df}
#' @param verbose If \code{TRUE}, the number of rows removed through filtering
#'   will be printed to the console. Default is \code{TRUE}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @importFrom tidyselect everything
#' @importFrom rlang .data
#' @importFrom utils write.csv
#'
#' @return List of model stats (in \code{data.frame}) and trained model object.
#'   Saves both to \code{model.save.folder} as well. To use optimally trained
#'   model for predictions, use tuned parameters from \code{$bestTune}
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' test.model <- ikeogu.2017 %>%
#'   dplyr::filter(study.name == "C16Mcal") %>%
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::select(sample.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   SaveModel(df = ., save.model = FALSE,
#'             autoselect.preprocessing = TRUE,
#'             model.name = "my_prediction_model",
#'             tune.length = 50, num.iterations = 10,
#'             wavelengths = 350:2500)
#' summary(test.model[1])
#' test.model[2]
#' }
SaveModel <- function(df,
                      save.model = TRUE,
                      autoselect.preprocessing = TRUE,
                      preprocessing.method = NULL,
                      model.save.folder = NULL,
                      model.name = "PredictionModel",
                      best.model.metric = "RMSE",
                      tune.length = 50,
                      model.method = "pls",
                      num.iterations = 10,
                      wavelengths = 740:1070,
                      stratified.sampling = TRUE,
                      cv.scheme = NULL,
                      trial1 = NULL,
                      trial2 = NULL,
                      trial3 = NULL,
                      verbose = TRUE
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

  if(!autoselect.preprocessing & is.null(preprocessing.method)){
    stop("Please select a preprocessing method (1-12) if not using autoselect.")
  }

  if(is.null(model.save.folder)){
    model.save.folder = getwd()
  }

  # Choose best preprocessing method and set up training set
  methods.list <- c("Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG",
                    "SNVSG", "SGD1", "SG.D1W5", "SG.D1W11", "SG.D2W5",
                    "SG.D2W11")

  if (!autoselect.preprocessing) {
    cat("Training model...\n")
    formatted.df <- DoPreprocessing(df = df, test.data = NULL,
                                    preprocessing.method = preprocessing.method,
                                    wavelengths = wavelengths)
    training.results <- TrainSpectralModel(df = formatted.df,
                                           num.iterations = num.iterations,
                                           test.data = NULL,
                                           tune.length = tune.length,
                                           model.method = model.method,
                                           output.summary = TRUE,
                                           return.model = TRUE,
                                           stratified.sampling = stratified.sampling,
                                           best.model.metric = best.model.metric,
                                           cv.scheme = cv.scheme,
                                           trial1 = trial1, trial2 = trial2,
                                           trial3 = trial3,
                                           rf.variable.importance = FALSE,
                                           split.test = FALSE,
                                           verbose = verbose)
    best.model <- training.results[[1]]
    best.model.stats <- training.results[[2]]
    best.model.stats <- best.model.stats %>%
      dplyr::mutate(Pretreatment = methods.list[preprocessing.method]) %>% # Create "Pretreatment" column
      dplyr::select(.data$Pretreatment, everything()) # move "Pretreatment" column to far left
    if(verbose) print(best.model.stats)
  } else {
    if(verbose) cat("Preprocessing...\n")
    # Preprocess data and use model statistics from training function to determine best model
    df.list <- DoPreprocessing(df = df, test.data = NULL,
                               preprocessing.method = 1:13,
                               wavelengths = wavelengths)

    # Set up empty list and data frame for training output
    model.list <- list()
    results.df <- as.data.frame(matrix(nrow = 13, ncol = 17))
    results.df[,1] <- methods.list

    for(i in 1:13){
      if (verbose)cat(paste0("Training model ", i, " of 13: (", methods.list[i], ").\n"))
      # Train model
      training.results.i <- TrainSpectralModel(df = df.list[[i]],
                                               num.iterations = num.iterations,
                                               test.data = NULL,
                                               tune.length = tune.length,
                                               model.method = model.method,
                                               output.summary = TRUE,
                                               return.model = TRUE, # Outputs list of c(model, model performance metrics)
                                               stratified.sampling = stratified.sampling,
                                               best.model.metric = best.model.metric,
                                               cv.scheme = cv.scheme,
                                               trial1 = trial1, trial2 = trial2,
                                               trial3 = trial3,
                                               rf.variable.importance = FALSE,
                                               split.test = FALSE,
                                               verbose = verbose)
      model.list[[i]] <- training.results.i[[1]] # Extract model from training output
      # Format output
      # Put pretreatment name in first column followed by means and standard deviations for each statistic
      results.df$Pretreatment[i] <- methods.list[i]
      spectacle.results.i <- training.results.i[[2]] %>%
        dplyr::select(.data$RMSEp:.data$R2sp)
      hyperparameter.results.i <- training.results.i[[2]] %>%
        dplyr::select(-(.data$Summary_type:.data$R2sp)) # works even if no hyperparameter columns
      results.df[i, 2:ncol(results.df)] <- data.frame(spectacle.results.i[1,], # row 1 is means
                                                      spectacle.results.i[2,], # row 2 is standard deviations
                                                      hyperparameter.results.i[1,]) # first row is values, second is just NA
    } # End loop of pretreatments

    colnames(results.df) <- c("Pretreatment", "RMSE", "Rsquared", "RPD",
                              "RPIQ", "CCC", "Bias", "SEP", "R2sp",
                              "RMSE.sd", "Rsquared.sd", "RPD.sd", "RPIQ.sd",
                              "CCC.sd", "Bias.sd", "SEP.sd", "R2sp.sd")

    # Use results data frame to determine best preprocessing technique
    best.type.num <- ifelse(best.model.metric == "RMSE",
                            which.min(results.df$RMSE),
                            which.max(results.df$Rsquared))

    if (verbose) {
      cat("\nTraining Summary:\n")
      print(results.df)
      cat(paste0("\nBest preprocessing technique: ",
                 results.df$Pretreatment[best.type.num], "\n"))
    }

    # Set chosen model as best.model for export
    best.model <- model.list[[best.type.num]]
    best.model.stats <- results.df[best.type.num,]

  } # End preprocessing if statement

  if(save.model){
    if(verbose){
      cat(paste0("\nSaving model and model statistics to ",
                 model.save.folder, ".\n"))
    }
    # Output stats to model.save.folder as 'model.name_stats.csv'
    write.csv(best.model.stats,
              file = paste0(model.save.folder, '/', model.name,
                            '_stats.csv'), row.names = FALSE)
    # Save model in save location as 'model.name.Rds'
    saveRDS(best.model, file = paste0(model.save.folder, '/',
                                      model.name, ".Rds"))
  }

  # Output list of model stats data frame and model
  output.list <- list(best.model.stats, best.model)
  return(output.list)
}
