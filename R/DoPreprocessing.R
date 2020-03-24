#' @title Preproccess spectral data according to user-designated method
#' @name DoPreprocessing
#' @description #TODO
#'
#'
#' @param df `data.frame` object containing spectral data. First column(s) (optional)
#' include metadata (with or without reference value column) followed by spectral columns.
#' Spectral column names must be formatted as "X" followed by wavelength
#' Include no other columns to right of spectra! No missing values permitted.
#' @param test.data `data.frame` object with same format as train.data. Will be appended to df
#       during preprocessing so that the same transformations are applied to each row. Default is NULL.
#' @param preprocessing.method Number or list of numbers 1:13 corresponding to desired pretreatment method(s):
#' * 1 = raw data (default)
#' * 2 = standard normal variate (SNV)
#' * 3 = SNV and first derivative
#' * 4 = SNV and second derivative
#' * 5 = first derivative
#' * 6 = second derivative
#' * 7 = Savitzky–Golay filter (SG)
#' * 8 = SNV and SG
#' * 9 = gap segment derivative (window size = 11)
#' * 10 = SG and first derivative (window size = 5)
#' * 11 = SG and first derivative (window size = 11)
#' * 12 = SG and second derivative (window size = 5)
#' * 13 = SG and second derivative (window size = 11)
#' @param wavelengths List of wavelengths represented by each column in `df`
#'
#' @importFrom prospectr standardNormalVariate savitzkyGolay gapDer
#'
#' @return Preprocessed `df` (or list of `data.frames`) with reference column intact
#' @export
#'
#' @examples
DoPreprocessing <- function(df,
                            test.data = NULL,
                            preprocessing.method = 1,
                            wavelengths = 740:1070
) {
  # TODO error handling
  # Format input data frames for processing
  # Combine training.data and test.data so that the same transformations are applied to all samples
  if (!is.null(test.data)) {
    # bind training to test for preprocessing
    df <- rbind(df, test.data)
  }

  # Remove rows with missing data (shouldn't be any, but this is just in case)
  df <- na.omit(df)

  # Split spectra from metadata turn spectra into matrix (spc)
  spc <- df %>% dplyr::select(starts_with("X")) %>% data.matrix()
  metadata <- df %>% dplyr::select(-starts_with("X"))

  # Add preprocessed spectral data to list
  processed.list <- list(spc, # 1. Raw data
                         # Scatter correction (to remove noise)
                         standardNormalVariate(spc), # 2. Standard normal variate (SNV)
                         t(diff(t(standardNormalVariate(spc)), differences = 1)), # 3. SNV + 1st derivative
                         t(diff(t(standardNormalVariate(spc)), differences = 2)), # 4. SNV + 2nd derivative
                         t(diff(t(spc), differences = 1)), # 5. First derivative
                         t(diff(t(spc), differences = 2)), # 6. Second derivative
                         # Smoothing filters
                         savitzkyGolay(spc, p = 2, w = 11, m = 0), # 7. Savitzky-Golay
                         savitzkyGolay(standardNormalVariate(spc), p = 2, w = 11, m = 0), # 8. SNV + Savitzky-Golay
                         gapDer(spc, m = 1, w = 11, s = 10), # 9. Savitzky-Golay + Gap-segment derivative (gapDer) algorithms
                         savitzkyGolay(spc, p = 2, w = 5, m = 1), # 10. Savitzky-Golay filter + 1st derivative - window size=5
                         savitzkyGolay(spc, p = 2, w = 11, m = 1), # 11. Savitzky-Golay filter + 1st derivative - window size=11
                         savitzkyGolay(spc, p = 2, w = 5, m = 2), # 12. Savitzky-Golay filter + 2nd derivative - window size=5
                         savitzkyGolay(spc, p = 2, w = 11, m = 2) # 13. Savitzky-Golay filter + 2nd derivative - window size=11
  )

  # Make each matrix in list into a data frame and add back reference column if present
  for(i in c(1:13)){
    df.to.add <- as.data.frame(processed.list[[i]])
    df.to.add <- cbind(metadata, df.to.add)
    processed.list[[i]] <- df.to.add
  }

  # Choose which to return
  if(length(preprocessing.method) == 1){
    # Return a single processed data frame if only one preprocessing method was chosen.
    processed <- processed.list[[preprocessing.method]]
  } else{
    # Return a list of processed data frames if more than one preprocessing method was chosen.
    processed <- processed.list[c(preprocessing.method)]
  }

  return(processed)
}
