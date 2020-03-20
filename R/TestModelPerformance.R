#' @title Wrapper that trains models based spectral data to predict reference values and reports
#     model performance statistics
#' @name TestModelPerformance
#' @description #TODO
#' @details Calls [DoPreprocessing()], [FormatCV()], and [TrainSpectralModel()] functions.
#' @author Jenna Hershberger
#'
#' @inheritParams FormatCV
#' @inheritParams TrainSpectralModel
#' @param train.data `data.frame` object of spectral data for input into a spectral prediction model.
#' First column contains unique identifiers, second contains reference values, followed by spectral
#' columns. Include no other columns to right of spectra! Column names of spectra must start with "X"
#' and reference column must be named "reference".
#' @param preprocessing If `TRUE`, 12 preprocessing methods will be applied and their performance
#' analyzed. If `FALSE`, input data is analyzed as is (raw). Default is `FALSE`.
#' @param wavelengths List of wavelengths represented by each column in `train.data`
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#'
#' @return `data.frame` with model performance statistics (RMSE, Rsquared, RPD, RPIQ, CCC, Bias, SE, K)
#' in summary format (2 rows, one with mean and one with standard deviation of all training iterations)
#' or in long format (number of rows = num.iterations). *Note* if `preprocessing = TRUE`, only the first
#' mean of summary statistics for all iterations of training are provided for each technique.
#' @export
#'
#' @examples
TestModelPerformance <- function(train.data,
                                 num.iterations,
                                 test.data = NULL,
                                 preprocessing = T,
                                 wavelengths = 740:1070,
                                 tune.length = 50,
                                 model.method = "pls",
                                 output.summary = T,
                                 rf.variable.importance = F,
                                 stratified.sampling = T,
                                 cv.scheme = NULL,
                                 trial1 = NULL,
                                 trial2 = NULL,
                                 trial3 = NULL,
                                 split.test = F
) {
  # Error handling
  if(!is.null(cv.scheme)){
    if(is.null(trial1)){
      stop("trial1 must be provided if using cv.scheme")
    }
    if(is.null(trial2)){
      stop("trial2 must be provided if using cv.scheme")
    }
    if(sum(colnames(trial1) != colnames(trial2))>0){
      stop("Column names must match for trial1 and trial2 if using cv.scheme")
    }
    if(!is.null(trial3) & sum(colnames(trial1) != colnames(trial3))>0){
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
    results.colnames <- c("Pretreatment", "RMSE", "Rsquared", "RPD", "RPIQ", "CCC", "Bias", "SE", "Spearman")
    # Add standard deviation columns if outputting a summary data frame
    if(output.summary){
      results.colnames <- append(results.colnames, c("RMSE.sd", "Rsquared.sd", "RPD.sd", "RPIQ.sd", "CCC.sd", "Bias.sd", "SE.sd", "Spearman.sd"))
    } else{
      # Add iteration column
      results.colnames <- c("Pretreatment", "Iteration", "RMSE", "Rsquared", "RPD", "RPIQ", "CCC", "Bias", "SE", "Spearman")
    }
    # Add hyperparameter columns
    # svmLinear requires no extra columns for tuned hyperparameter results, pls and svmRadial require 1, and rf requires 2
    if(model.method == "pls" | model.method == "svmRadial"){
      results.colnames <- append(results.colnames, "Best.ncomp")
    } else if(model.method == "rf"){
      results.colnames <- append(results.colnames, c("Best.ntree", "Best.mtry"))
    }
    # Put it together
    results.df <- as.data.frame(matrix(ncol = length(results.colnames), nrow = nrow.results, NA))
    colnames(results.df) <- results.colnames

    # Then do preprocessing on everything
    cat("Preprocessing initiated.\n")
    if(!is.null(cv.scheme)){
      train.data <- rbind(trial1, trial2, trial3)
    }
    df.list <- DoPreprocessing(df = train.data, test.data = test.data, preprocessing.method = 1:13,
                               wavelengths = wavelengths) # TODO make sure this works and retains genotype

    # Training loop
    cat("Training models...\n")
    for (i in c(1:13)) { # TODO: allow output of all iterations from each method (not just summary)
      methods.list <- c("Raw_data", "SNV", "SNV1D", "SNV2D", "D1", "D2", "SG", "SNVSG", "SGD1", "SG.D1W5",
                        "SG.D1W11", "SG.D2W5", "SG.D2W11")
      cat(paste("Working on method", i, "of 13:", methods.list[i], "\n", sep = " "))
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
        processed.trial2 <- df.list[[i]][(nrow(trial1)+1):(nrow(trial1)+nrow(trial2)),]
        processed.trial3 <- df.list[[i]][(nrow(trial1)+nrow(trial2)+1):nrow(train.data),]
      }
      # Fit models for each pretreatment and output results
      training.results <- TrainSpectralModel(df = processed.train.data, num.iterations = num.iterations,
                                             test.data = processed.test.data, output.summary = output.summary,
                                             tune.length = tune.length, model.method = model.method,
                                             rf.variable.importance = rf.variable.importance,
                                             stratified.sampling = stratified.sampling,
                                             return.model = F, trial1 = processed.trial1,
                                             trial2 = processed.trial2, trial3 = processed.trial3,
                                             split.test = split.test)
      # Format output
      if(rf.variable.importance){
        # Separate variable importance from model performance results
        rf.importance.df <- training.results[2]
        training.results <- training.results[1]
      }

      if(output.summary){
        # Put pretreatment name in first column followed by means and standard deviations for each statistic
        results.df$Pretreatment[i] <- methods.list[i]
        spectacle.results <- training.results %>% dplyr::select(.data$RMSE:.data$Spearman)
        hyperparameter.results <- training.results %>%
          dplyr::select(-(.data$Summary_type:.data$Spearman)) # works even if no hyperparameter columns
        results.df[i, 2:ncol(results.df)] <- data.frame(spectacle.results[1,], # row 1 is means
                                                        spectacle.results[2,], # row 2 is standard deviations
                                                        hyperparameter.results[1,]) # first row is values, second is just NA
      } else{ # output each iteration
        # Starting row = Pretreatment number - 1 * number of iterations
        # Raw data example// ((1-1) * 50) + 1 = (0*50) +1 = 1
        # SNV example// ((2-1*50)+1) = 51
        starting.row <- ((i-1) * num.iterations) + 1
        results.df$Pretreatment[starting.row:(starting.row + num.iterations - 1), 1] <- methods.list[i]
        results.df[starting.row:(starting.row + num.iterations - 1), -1] <- training.results
      }
    } # End of loop


  } else{ # No preprocessing techniques applied
    cat("Preprocessing skipped.\n")

    # Fit models for each pretreatment and output results
    cat("Training model...\n")
    results.df <- TrainSpectralModel(df = train.data,
                                     num.iterations = num.iterations,
                                     test.data = test.data,
                                     output.summary = output.summary,
                                     tune.length = tune.length,
                                     model.method = model.method,
                                     rf.variable.importance = rf.variable.importance,
                                     stratified.sampling = stratified.sampling,
                                     return.model = F, cv.scheme = cv.scheme,
                                     trial1 = trial1, trial2 = trial2, trial3 = trial3)
    if(rf.variable.importance){
      rf.importance.df <- results.df[2]
      results.df <- results.df[1]
    }
    results.df <- as.data.frame(results.df)
  }
  ifelse(rf.variable.importance, return(list(results.df, rf.importance.df)), return(results.df))
  }