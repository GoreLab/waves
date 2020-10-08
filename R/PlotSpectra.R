#'@title Plot spectral data, highlighting outliers as identified using
#'  Mahalanobis distance
#'@description Generates a \code{\link[ggplot2]{ggplot}} object of given
#'  spectra, with wavelength on the x axis and given spectral values on the y.
#'  Mahalanobis distance is used to calculate outliers, which are both
#'  identified on the plot. Rows from the original dataframe are printed to the
#'  console for each outlier that is identified.
#'@author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams FilterSpectra
#'@param input.df \code{data.frame} object containing columns of spectra.
#'  Spectral columns must be labeled with an "X" and then the wavelength
#'  (example: "X740" = 740nm). Left-most column must be unique ID. May also
#'  contain columns of metadata between the unique ID and spectral columns.
#'  Cannot contain any missing values
#'@param wavelengths List of wavelengths (numerical format) represented by each
#'  spectral column in \code{input.df}
#'@param num.col.before.spectra Number of columns to the left of the spectral
#'  matrix (including unique ID). Default is 1.
#'
#'@importFrom dplyr mutate distinct
#'@importFrom ggplot2 ggplot aes geom_line theme_minimal labs scale_color_manual
#'@importFrom wesanderson wes_palette
#'@importFrom tidyr gather
#'@importFrom stringr str_extract
#'@importFrom rlang .data
#'
#'@return If verbose, prints unique ID and metadata for rows identified as outliers.
#'  Returns plot of spectral data with non-outliers in blue and outliers in red.
#'  X-axis is wavelengths and y-axis is spectral values.
#'@export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::rename(unique.id = sample.id) %>%
#'   dplyr::select(unique.id, dplyr::everything(), -TCC) %>%
#'   na.omit() %>%
#'   PlotSpectra(input.df = .,
#'               wavelengths = 350:2500,
#'               num.col.before.spectra = 5,
#'               window.size = 15)
#' }
PlotSpectra <- function(input.df,
                        wavelengths,
                        num.col.before.spectra = 1,
                        window.size = 10,
                        verbose = TRUE
                        ){

    # Strip off non-spectral columns
  spectra <- input.df[, (num.col.before.spectra + 1):ncol(input.df)]

  # Error handling
  # mahalanobis function does not allow missing values or non-numeric data
  if(nrow(spectra) != nrow(na.omit(spectra))){
    stop("Input data frame cannot contain missing values! Remove them and try again.")
  }

  # Make sure spectral columns start with X and match number of wavelengths provided
  if(input.df %>% dplyr::select(starts_with("X")) %>% ncol() != length(wavelengths)){
    stop("Spectral column names must start with an 'X' and must match the number of wavelengths provided.")
  }

  # Calculate outlier cutoff
  chisq95 <- qchisq(.95, df = length(wavelengths))

  # Calculate Mahalanobis distribution for each scan and identify outliers
  filtered.df <- FilterSpectra(df = input.df, filter = FALSE, return.distances = TRUE,
                               num.col.before.spectra = num.col.before.spectra,
                               window.size = window.size, verbose = FALSE) %>%
    mutate(Outlier = ifelse(.data$h.distances > chisq95, TRUE, FALSE))

  # Prepare data frame for plotting
  hdists.df <- filtered.df %>%
    dplyr::select(1, .data$h.distances, .data$Outlier, starts_with("X")) %>%
    gather(key = "wl", value = "s.value", starts_with("X")) %>%
    mutate(wl = as.numeric(str_extract(.data$wl, "\\-*\\d+\\.*\\d*")))

  # Create plot
  hdist.chisq95.plot <- ggplot(data = hdists.df,
                               aes(x = .data$wl,
                                   y = .data$s.value,
                                   group = .data$unique.id,
                                   color = .data$Outlier)) +
    geom_line(alpha = .5) +
    geom_line(data = subset(.data$hdists.df, .data$Outlier == T), alpha = .7) +
    scale_color_manual(values = wes_palette(name = "Zissou1")[c(1,5)], name = "Outlier?") +
    theme_minimal() +
    labs(title = paste0("Chi-Square (", length(wavelengths)," df) 95% cutoff for Mahalanobis distance"),
         subtitle = "Raw spectra", x = "Wavelength", y = "Spectral Value")

  # Print metadata for each outlier
  if (verbose) {
    if (sum(hdists.df$Outlier > 0)) {
      cat("Outliers:\n")
      print(
        filtered.df %>% dplyr::filter(.data$h.distances > chisq95) %>%
          dplyr::select(-starts_with("X")) %>% distinct()
      )
    } else{
      cat("No outliers detected.\n")
    }
  }

  return(hdist.chisq95.plot)
}
