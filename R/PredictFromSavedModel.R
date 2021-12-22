#' @title Use provided model object to predict trait values with input dataset
#' @name PredictFromSavedModel
#' @description Loads an existing model and cross-validation performance
#'   statistics (created with \code{\link{SaveModel}}) and makes predictions
#'   based on new spectra.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#' @inheritParams TestModelPerformance
#' @param input.data \code{data.frame} object of spectral data for input into a
#'   spectral prediction model. First column contains unique identifiers
#'   followed by spectral columns. Include no other columns to right of spectra!
#'   Column names of spectra must start with "X".
#' @param model.stats.location String containing file path (including file name)
#'   to save location of "(model.name)_stats.csv" as output from the SaveModel
#'   function.
#' @param model.location String containing file path (including file name) to
#'   location where the trained model ("(model.name).Rds") was saved as output
#'   by the \code{\link{SaveModel}} function.
#' @importFrom stats predict
#' @importFrom utils read.csv
#'
#' @return \code{data.frame} object of predictions for each sample (row). First
#'   column is unique identifier supplied by \code{input.data} and second is
#'   predicted values
#' @export
#'
#' @examples
#' \dontrun{
#' ikeogu.2017 %>%
#'   dplyr::select(sample.id, dplyr::starts_with("X")) %>%
#'   PredictFromSavedModel(
#'     input.data = .,
#'     model.stats.location = paste0(
#'       getwd(),
#'       "/my_model_stats.csv"
#'     ),
#'     model.location = paste0(getwd(), "/my_model.Rds")
#'   )
#' }
#'
PredictFromSavedModel <- function(input.data,
                                  model.stats.location,
                                  model.location,
                                  model.method = "pls") {

  # Load model and model statistics ---------------------------
  model.stats <- read.csv(model.stats.location)
  model.object <- readRDS(model.location)
  final.model <- model.object
  # Match best preprocessing method with index number ---------------------------
  best.preprocessing.num <- match(
    model.stats$Pretreatment[1],
    c(
      "Raw_data", "SNV", "SNV1D", "SNV2D", "D1",
      "D2", "SG", "SNVSG", "SGD1", "SG.D1W5",
      "SG.D1W11", "SG.D2W5", "SG.D2W11"
    )
  )

  # Use DoPreprocessing function to format input.data and preprocess if needed ---------------------------
  preprocessed <- DoPreprocessing(
    df = input.data, test.data = NULL,
    preprocessing.method = best.preprocessing.num
  )

  # Predict values using imported model, preprocessed/formatted input data, and method of choice ---------------------------
  if (model.method == "pls") {
    # Extract best number of components
    best.ncomp <- model.stats$best.ncomp[1]
    # Get predictions
    predicted.values <- as.numeric(predict(final.model,
      newdata = as.matrix(preprocessed[2:ncol(preprocessed)]),
      ncomp = best.ncomp
    ))
  } else if (model.method == "svmLinear") {
    predicted.values <- as.numeric(predict(final.model, newdata = preprocessed))
  } else if (model.method == "svmRadial") {
    predicted.values <- as.numeric(predict(final.model, newdata = preprocessed))
  } else if (model.method == "rf") {
    best.ntree <- final.model$ntree
    best.mtry <- final.model$mtry
    predicted.values <- as.numeric(predict(final.model,
      newdata = preprocessed,
      ntree = best.ntree,
      mtry = best.mtry
    ))
  }

  # Bind unique identifiers from the input data to the predicted values ---------------------------
  predicted.df <- cbind(input.data[, 1], data.frame(predicted.values))
  colnames(predicted.df) <- c(colnames(input.data)[1], "predicted.values")

  return(predicted.df)
}
