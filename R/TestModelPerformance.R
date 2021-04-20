#' @title Test the performance of spectral models
#' @name TestModelPerformance
#' @description Wrapper that trains models based spectral data to predict
#'   reference values and reports model performance statistics
#'
#' @details Calls \code{\link{DoPreprocessing}}, \code{\link{FormatCV}},
#' and \code{\link{TrainSpectralModel}} functions.
#'
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams FormatCV
#' @inheritParams TrainSpectralModel
#' @param train.data \code{data.frame} object of spectral data for input into a
#'   spectral prediction model. First column contains unique identifiers, second
#'   contains reference values, followed by spectral columns. Include no other
#'   columns to right of spectra! Column names of spectra must start with "X"
#'   and reference column must be named "reference".
#' @param preprocessing If \code{TRUE}, 12 preprocessing methods will be applied
#'   and their performance analyzed. If \code{FALSE}, input data is analyzed as
#'   is (raw). Default is \code{FALSE}.
#' @param wavelengths List of wavelengths represented by each column in
#'   \code{train.data}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @return \code{data.frame} with model performance statistics in summary format
#'   (2 rows, one with mean and one with standard deviation of all training
#'   iterations) or in long format (number of rows = num.iterations).
#'   \strong{Note} if \code{preprocessing = TRUE}, only the first mean of
#'   summary statistics for all iterations of training are provided for each
#'   technique.
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
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::rename(unique.id = sample.id) %>%
#'   dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
#'   na.omit() %>%
#'   TestModelPerformance(train.data = .,
#'                        tune.length = 3,
#'                        num.iterations = 3,
#'                        preprocessing = FALSE,
#'                        wavelengths = 350:2500)
#' }
TestModelPerformance <- function(train.data,
                                 num.iterations,
                                 test.data = NULL,
                                 preprocessing = TRUE,
                                 wavelengths = 740:1070,
                                 tune.length = 50,
                                 model.method = "pls",
                                 output.summary = TRUE,
                                 rf.variable.importance = FALSE,
                                 stratified.sampling = TRUE,
                                 cv.scheme = NULL,
                                 trial1 = NULL,
                                 trial2 = NULL,
                                 trial3 = NULL,
                                 split.test = FALSE,
                                 verbose = TRUE
                                 ) {
  # Error handling
  if(!is.null(cv.scheme)){
    if(is.null(trial1)){
      stop("trial1 must be provided if using cv.scheme")
    }
    if(is.null(trial2)){
      stop("trial2 must be provided if using cv.scheme")
    }
    if(sum(colnames(trial1) != colnames(trial2)) > 0){
      stop("Column names must match for trial1 and trial2 if using cv.scheme")
    }
    if(!is.null(trial3) & sum(colnames(trial1) != colnames(trial3)) > 0){
      stop("Column names must match for trial1, trial2, and trial3 if using cv.scheme and including trial3")
    }
    train.data <- trial1
  }

  num.col.before.reference <- ifelse(!is.null(cv.scheme), 2, 1)
  if(length(wavelengths) != ncol(train.data) - (num.col.before.reference + 1)) {
    if(!is.null(cv.scheme)){
      stop("Number of spectral columns in train.data (ncol(train.data) - 3) must match number of wavelengths")
    } else{
      stop("Number of spectral columns in train.data (ncol(train.data) - 2) must match number of wavelengths")
    }
  }

  if(nrow(train.data) != nrow(na.omit(train.data))) {
    stop("Training data cannot contain missing values.")
  }

  if(!is.null(test.data) && (nrow(test.data) != nrow(na.omit(test.data)))){
    stop("Test data cannot contain missing values. \nEither omit missing values or exclude training data (
         set as NULL).")
  }

  if((rf.variable.importance & model.method != "rf")){
    stop('model.method must be "rf" if rf.variable.importance is TRUE')
  }

  n.train <- nrow(train.data)
  n.test <- ifelse(is.null(test.data), 0, nrow(test.data))

  if(preprocessing){
    # Format empty data frame for results
    nrow.results <- ifelse(output.summary, 13, 13 * num.iterations)
    # Set column names
    results.colnames <- c("Pretreatment", "RMSEp", "R2p", "RPD", "RPIQ", "CCC",
                          "Bias", "SEP", "RMSEcv", "R2cv", "R2sp")
    # Add standard deviation columns if outputting a summary data frame
    if(output.summary){
      results.colnames <- append(results.colnames, c("RMSEp.sd", "R2p.sd",
                                                     "RPD.sd", "RPIQ.sd",
                                                     "CCC.sd", "Bias.sd",
                                                     "SEP.sd", "RMSEcv.sd",
                                                     "R2cv.sd", "R2sp.sd"))
    } else{
      # Add iteration column
      results.colnames <- c("Pretreatment", "Iteration", "RMSEp", "R2p", "RPD",
                            "RPIQ", "CCC", "Bias", "SEP", "RMSEcv", "R2cv",
                            "R2sp")
    }
    # Add hyperparameter columns
    # svmLinear requires no extra columns for tuned hyperparameter results, pls and svmRadial require 1, and rf requires 2
    if(model.method == "pls" | model.method == "svmRadial"){
      results.colnames <- append(results.colnames, "Best.ncomp")
    } else if(model.method == "rf"){
      results.colnames <- append(results.colnames, c("Best.ntree", "Best.mtry"))
    }
    # Put it together
    results.df <- as.data.frame(matrix(ncol = length(results.colnames),
                                       nrow = nrow.results, NA))
    colnames(results.df) <- results.colnames

    # Then do preprocessing on everything
    if(verbose){
      cat("Preprocessing initiated.\n")
    }
    if(!is.null(cv.scheme)){
      train.data <- rbind(trial1, trial2, trial3)
    }
    df.list <- DoPreprocessing(df = train.data, test.data = test.data,
                               preprocessing.method = 1:13,
                               wavelengths = wavelengths)

    # Training loop
    if(verbose){
      cat("Training models...\n")
    }

    for (i in c(1:13)) {
      methods.list <- c("Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG",
                        "SNVSG", "SGD1", "SG.D1W5",
                        "SG.D1W11", "SG.D2W5", "SG.D2W11")
      if(verbose){
        cat(paste("Working on method", i, "of 13:", methods.list[i], "\n",
                  sep = " "))
      }
      # To access a specfic pretreatment, use df.list[[i]] where i is the number of the pretreatment in methods.list above
      # Extract test dataset from full processed data frame
      processed.train.data <- df.list[[i]][1:n.train,]
      if(n.test == 0){
        processed.test.data <- NULL
      } else{
        processed.test.data <- df.list[[i]][(n.train + 1):(n.train + n.test),]
      }

      if(!is.null(cv.scheme)){
        processed.trial1 <- df.list[[i]][1:nrow(trial1),]
        processed.trial2 <- df.list[[i]][(nrow(trial1) + 1):(nrow(trial1) + nrow(trial2)),]
        processed.trial3 <- df.list[[i]][(nrow(trial1) + nrow(trial2) + 1):nrow(train.data),]
      }
      # Fit models for each pretreatment and output results
      training.results <- TrainSpectralModel(df = processed.train.data,
                                             num.iterations = num.iterations,
                                             test.data = processed.test.data,
                                             output.summary = output.summary,
                                             tune.length = tune.length,
                                             model.method = model.method,
                                             stratified.sampling = stratified.sampling,
                                             return.model = F,
                                             trial1 = processed.trial1,
                                             trial2 = processed.trial2,
                                             trial3 = processed.trial3,
                                             rf.variable.importance = rf.variable.importance,
                                             split.test = split.test,
                                             verbose = verbose)
      # Format output
      if(rf.variable.importance){
        # Separate variable importance from model performance results
        training.results.rf.importance.i <- training.results$RF.variable.importance
        training.results <- training.results$model.performance
      }

      if(output.summary){
        # Put pretreatment name in first column followed by means and standard deviations for each statistic
        results.df$Pretreatment[i] <- methods.list[i]
        spectacle.results <- training.results %>%
          dplyr::select(.data$RMSEp:.data$R2sp)
        hyperparameter.results <- training.results %>%
          dplyr::select(-(.data$Summary_type:.data$R2sp)) # works even if no hyperparameter columns
        results.df[i, 2:ncol(results.df)] <- data.frame(spectacle.results[1,], # row 1 is means
                                                        spectacle.results[2,], # row 2 is standard deviations
                                                        hyperparameter.results[1,]) # first row is values, second is just NA
      } else{ # output each iteration
        # Starting row = Pretreatment number - 1 * number of iterations
        # Raw data example// ((1-1) * 50) + 1 = (0*50) +1 = 1
        # SNV example// ((2-1*50)+1) = 51
        starting.row <- ((i-1) * num.iterations) + 1
        ending.row <- starting.row + num.iterations - 1
        results.df$Pretreatment[(starting.row:ending.row)] <- methods.list[i]
        results.df[(starting.row:ending.row), -1 ] <- training.results

        if(rf.variable.importance){
          training.results.rf.importance.i <- cbind(methods.list[i], training.results.rf.importance.i)
          colnames(training.results.rf.importance.i)[1] <- "Pretreatment"
          training.results.rf.importance.i %<>%
            tidyr::pivot_longer(.data, cols = starts_with("X"), names_to = "Wavelength", values_to = "RF.importance")
          #print(training.results.rf.importance.i) #TODO
          if(i == 1){
            rf.importance.df <- training.results.rf.importance.i
          } else{
            rf.importance.df <- rbind(rf.importance.df, training.results.rf.importance.i)
          }
          #print(rf.importance.df) # TODO
        }

      }
    } # End of loop


  } else{ # No preprocessing techniques applied
    if(verbose){
      cat("Preprocessing skipped.\n")

      # Fit models for each pretreatment and output results
      cat("Training model...\n")
    }
    results.df <- TrainSpectralModel(df = train.data,
                                     num.iterations = num.iterations,
                                     test.data = test.data,
                                     output.summary = output.summary,
                                     tune.length = tune.length,
                                     model.method = model.method,
                                     stratified.sampling = stratified.sampling,
                                     return.model = F, cv.scheme = cv.scheme,
                                     trial1 = trial1, trial2 = trial2,
                                     trial3 = trial3,
                                     rf.variable.importance = rf.variable.importance,
                                     split.test = split.test,
                                     verbose = verbose
                                     )
    if(rf.variable.importance){
      return(results.df)
    }
  } # End no preprocessing

  results.df <- as.data.frame(results.df)

  if(rf.variable.importance){
    rf.importance.df %<>%
      tidyr::pivot_wider(id_cols = .data$Pretreatment:.data$Iteration, names_from = .data$Wavelength, values_from = .data$RF.importance)
    return(list(model.performance = results.df,
                RF.variable.importance = rf.importance.df))
  } else{
    return(results.df)
  }

  }
