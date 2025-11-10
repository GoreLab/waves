#' @title Pretreat spectral data according to user-designated method
#' @name pretreat_spectra
#' @description Pretreatment, also known as preprocessing, is often used to
#'   increase the signal to noise ratio in vis-NIR datasets. The \emph{waves}
#'   function \code{pretreat_spectra} applies common spectral pretreatment
#'   methods such as standard normal variate and the Savitzky-Golay filter.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#' @param df \code{data.frame} object containing spectral data. First column(s)
#'   (optional) include metadata (with or without reference value column)
#'   followed by spectral columns. Spectral column names must be formatted as
#'   "X" followed by wavelength Include no other columns to right of spectra! No
#'   missing values permitted.
#' @param test.data \code{data.frame} object with same format as train.data.
#'   Will be appended to \code{df} during pretreatment so that the same
#'   transformations are applied to each row. Default is \code{NULL}.
#' @param pretreatment Number or list of numbers 1:13 corresponding to
#'   desired pretreatment method(s):
#' \enumerate{
#'   \item Raw data (default)
#'   \item Standard normal variate (SNV)
#'   \item SNV and first derivative
#'   \item SNV and second derivative
#'   \item First derivative
#'   \item Second derivative
#'   \item Savitzky–Golay filter (SG)
#'   \item SNV and SG
#'   \item Gap-segment derivative (window size = 11)
#'   \item SG and first derivative (window size = 5)
#'   \item SG and first derivative (window size = 11)
#'   \item SG and second derivative (window size = 5)
#'   \item SG and second derivative (window size = 11)
#' }
#' @param wavelengths DEPRECATED \code{wavelengths} is no
#'   longer supported; this information is now inferred from \code{df}
#'   column names
#' @param preprocessing.method DEPRECATED \code{preprocessing.method}
#'   has been renamed "pretreatment"
#'
#' @importFrom prospectr standardNormalVariate savitzkyGolay gapDer
#' @importFrom tidyselect starts_with
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#' @importFrom lifecycle is_present deprecate_warn deprecated
#'
#' @return Pretreated \code{df}` (or list of \code{data.frame}s) with
#'   reference column intact
#' @export
#'
#' @examples
#' pretreat_spectra(df = ikeogu.2017, pretreatment = 3)[1:5, 1:5]
pretreat_spectra <- function(df,
                             test.data = NULL,
                             pretreatment = 1,
                             preprocessing.method = lifecycle::deprecated(),
                             wavelengths = lifecycle::deprecated()) {
  # Format input data frames for processing. Combine training.data and test.data
  # so that the same transformations are applied to all samples
  if (!is.null(test.data)) {
    # bind training to test for pretreatment
    df <- rbind(df, test.data)
  }

  if (lifecycle::is_present(preprocessing.method)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "pretreat_spectra(preprocessing.method)",
      with = "pretreat_spectra(pretreatment)"
    )
    pretreatment <- preprocessing.method
  }

  if (lifecycle::is_present(wavelengths)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "pretreat_spectra(wavelengths)",
      details = "Wavelength specification is now inferred from column names."
    )
  }

  # Remove rows with missing spectral data
  # (shouldn't be any, but this is just in case)
  df <- df %>% tidyr::drop_na(tidyselect::starts_with("X"))

  # Split spectra from metadata turn spectra into matrix (spc)
  spc <- df %>%
    dplyr::select(tidyselect::starts_with("X")) %>%
    data.matrix()
  metadata <- df %>% dplyr::select(-tidyselect::starts_with("X"))

  # Add preprocessed spectral data to list
  processed.list <- list(
    # 1. Raw data
    Raw_data = spc,
    # Scatter correction (to remove noise)
    # 2. Standard normal variate (SNV)
    SNV = standardNormalVariate(spc),
    # 3. SNV + 1st derivative
    SNV1D = t(diff(t(standardNormalVariate(spc)), differences = 1)),
    # 4. SNV + 2nd derivative
    SNV2D = t(diff(t(standardNormalVariate(spc)), differences = 2)),
    # 5. First derivative
    D1 = t(diff(t(spc), differences = 1)),
    # 6. Second derivative
    D2 = t(diff(t(spc), differences = 2)),
    # Smoothing filters
    # 7. Savitzky-Golay
    SG = savitzkyGolay(spc, p = 2, w = 11, m = 0),
    # 8. SNV + Savitzky-Golay
    SNVSG = savitzkyGolay(standardNormalVariate(spc), p = 2, w = 11, m = 0),
    # 9. Savitzky-Golay + Gap-segment derivative (gapDer) algorithms
    SGD1 = gapDer(spc, m = 1, w = 11, s = 9),
    # 10. Savitzky-Golay filter + 1st derivative - window size = 5
    SG.D1W5 = savitzkyGolay(spc, p = 2, w = 5, m = 1),
    # 11. Savitzky-Golay filter + 1st derivative - window size = 11
    SG.D1W11 = savitzkyGolay(spc, p = 2, w = 11, m = 1),
    # 12. Savitzky-Golay filter + 2nd derivative - window size = 5
    SG.D2W5 = savitzkyGolay(spc, p = 2, w = 5, m = 2),
    # 13. Savitzky-Golay filter + 2nd derivative - window size = 11
    SG.D2W11 = savitzkyGolay(spc, p = 2, w = 11, m = 2)
  )

  # Make each matrix in list into a data frame and add back metadata and
  # reference column if present
  processed.meta.list <- lapply(
    seq_along(processed.list),
    function(x) cbind(metadata, processed.list[[x]])
  )
  names(processed.meta.list) <- names(processed.list)

  # Choose which to return
  if (length(pretreatment) == 1) {
    # Return a single processed data frame if only one pretreatment was chosen.
    processed <- processed.meta.list[[pretreatment]]
  } else {
    # Return a list of processed data frames if more
    # than one pretreatment was chosen.
    processed <- processed.meta.list[c(pretreatment)]
  }

  return(processed)
}
