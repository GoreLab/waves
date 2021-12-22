#' @title Save spectral prediction model and model performance statistics
#' @name save_model
#' @description Given a set of pretreatment methods, saves the best spectral
#' prediction model and model statistics to \code{model.save.folder} as
#' \code{model.name.Rds} and \code{model.name_stats.csv} respectively. If only
#' one pretreatment method is supplied, results from that method are stored.
#' @details Wrapper that uses \code{\link{pretreat_spectra}},
#'   \code{\link{format_cv}}, and \code{\link{train_spectra}} functions.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams test_spectra
#' @inheritParams train_spectra
#' @inheritParams pretreat_spectra
#' @param save.model If \code{TRUE}, the trained model will be saved in .Rds
#'   format to the location specified by \code{model.save.folder}. If
#'   \code{FALSE}, the best model will be output by the function but will not
#'   save to a file. Default is \code{TRUE}.
#' @param model.save.folder Path to folder where model will be saved. If not
#'   provided, will save to working directory.
#' @param model.name Name that model will be saved as in
#'   \code{model.save.folder}. Default is "PredictionModel".
#' @param autoselect.preprocessing `r lifecycle::badge("deprecated")`
#'   \code{autoselect.preprocessing = FALSE} is no longer supported. If
#'   multiple pretreatment methods are supplied, the best will be automatically
#'   selected as the model to be saved.
#'
#' @importFrom utils write.csv
#' @importFrom rlang abort
#'
#' @return List of model stats (in \code{data.frame}) and trained model object.
#'   If the parameter \code{save.model} is TRUE, both objects are saved to
#'   \code{model.save.folder}. To use the optimally trained model for
#'   predictions, use tuned parameters from \code{$bestTune}.
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' test.model <- ikeogu.2017 %>%
#'   dplyr::filter(study.name == "C16Mcal") %>%
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::select(sample.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   save_model(
#'     df = .,
#'     save.model = FALSE,
#'     pretreatment = 1:13,
#'     model.name = "my_prediction_model",
#'     tune.length = 50,
#'     num.iterations = 10
#'   )
#' summary(test.model$best.model)
#' test.model$best.model.stats
#' }
save_model <- function(df,
                      save.model = TRUE,
                      pretreatment = 1,
                      model.save.folder = NULL,
                      model.name = "PredictionModel",
                      best.model.metric = "RMSE",
                      k.folds = 5,
                      tune.length = 50,
                      model.method = "pls",
                      num.iterations = 10,
                      stratified.sampling = TRUE,
                      cv.scheme = NULL,
                      trial1 = NULL,
                      trial2 = NULL,
                      trial3 = NULL,
                      verbose = TRUE,
                      wavelengths = deprecated(),
                      autoselect.preprocessing = deprecated(),
                      preprocessing.method = deprecated()) {

  # Deprecate warnings
  if(lifecycle::is_present(wavelengths)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "save_model(wavelengths)",
      details = "Wavelength specification is now inferred from column names.")
  }

  if(lifecycle::is_present(autoselect.preprocessing)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "save_model(autoselect.preprocessing)",
      details = "If multiple pretreatment methods are supplied, the best will be selected automatically.")
  }

  if(lifecycle::is_present(preprocessing.method)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "save_model(preprocessing.method)",
      with = "save_model(pretreatment)")
  }

  # Error handling
  if (!(best.model.metric %in% c("RMSE", "Rsquared"))) {
    rlang::abort('best.model.metric must be either "RMSE" or "Rsquared"')
  }

  if (nrow(df) != nrow(na.omit(df))) {
    rlang::abort("Training data cannot contain missing values.")
  }

  if (!is.character(model.name)) {
    rlang::abort("model.name must be a string!")
  }

  if (is.null(model.save.folder)) {
    model.save.folder <- getwd()
  }

  # Choose best pretreatment method and set up training set
  methods.list <- c(
    "Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG",
    "SNVSG", "SGD1", "SG.D1W5", "SG.D1W11", "SG.D2W5",
    "SG.D2W11"
  )

  training.results <- test_spectra(
    train.data = df,
    num.iterations = num.iterations,
    test.data = NULL,
    pretreatment = pretreatment,
    k.folds = k.folds,
    tune.length = tune.length,
    model.method = model.method,
    stratified.sampling = stratified.sampling,
    best.model.metric = best.model.metric,
    cv.scheme = cv.scheme,
    trial1 = trial1,
    trial2 = trial2,
    trial3 = trial3,
    split.test = FALSE,
    verbose = verbose
  )

  if (length(pretreatment) == 1) {
    best.model <- training.results$model
    best.model.stats <- training.results$summary.model.performance
    if (verbose) print(best.model.stats)
  }

  if (length(pretreatment) > 1) {
    # Use results data frame to determine best pretreatment technique
    results.df <- training.results$summary.model.performance
    best.type.num <- ifelse(best.model.metric == "RMSE",
                            which.min(results.df$RMSE),
                            which.max(results.df$R2p)
    )
    # Set chosen model as best.model for export
    best.model <- training.results$model[[best.type.num]]
    best.model.stats <- results.df[best.type.num, ]

    if (verbose) {
      cat("\nTraining Summary:\n")
      print(results.df)
      cat(paste0(
        "\nBest pretreatment technique: ",
        results.df$Pretreatment[best.type.num], "\n"
      ))
    }
  } # End multiple pretreatments if statement


  if (save.model) {
    if (verbose) {
      cat(paste0(
        "\nSaving model and model statistics to ",
        model.save.folder, ".\n"
      ))
    }
    # Output stats to model.save.folder as 'model.name_stats.csv'
    write.csv(best.model.stats,
              file = paste0(
                model.save.folder, "/", model.name,
                "_stats.csv"
              ), row.names = FALSE
    )
    # Save model in save location as 'model.name.Rds'
    saveRDS(best.model, file = paste0(
      model.save.folder, "/",
      model.name, ".Rds"
    ))
  }

  # Output list of model stats data frame and model
  output.list <- list(
    best.model = best.model,
    best.model.stats = best.model.stats
  )
  return(output.list)
}
