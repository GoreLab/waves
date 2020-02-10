#' @title Plot spectral data, highlighting outliers as identified using Mahalanobis distance
#' @description TODO
#' @author Jenna Hershberger
#'
#' @param input.df `data.frame` object containing columns of spectra. Spectral columns must be labeled
#' with an "X" and then the wavelength (example: "X740" = 740nm). Left-most column must be unique ID.
#' May also contain columns of metadata between the unique ID and spectral columns. Cannot contain
#' any missing values
#' @param wavelengths List of wavelengths (numerical format) represented by each spectral column in `input.df`
#' @param num.col.before.spectra Number of columns to the left of the spectral matrix (including unique ID).
#'
#' @importFrom dplyr mutate distinct
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs scale_color_manual
#' @importFrom wesanderson wes_palette
#' @importFrom tidyr gather
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#'
#' @return Prints unique ID and metadata for rows identified as outliers. Returns plot of spectral
#' data with non-outliers in blue and outliers in red. X-axis is wavelengths and y-axis is spectral values.
#' @export
#'
#' @examples
PlotSpectra <- function(input.df, wavelengths, num.col.before.spectra){
  # Error handling
  # Mahalanobis function does not allow missing values or non-numeric data
  if(nrow(input.df) != nrow(na.omit(input.df))){
    stop("Input data frame cannot contain missing values! Remove them and try again.")
  }

  # Make sure spectral columns start with X and match number of wavelengths provided
  if(input.df %>% dplyr::select(starts_with("X")) %>% ncol() != length(wavelengths)){
    stop("Spectral column names must start with and 'X' and must match the number of wavelengths provided.")
  }

  # Calculate outlier cutoff
  chisq95 <- qchisq(.95, df = length(wavelengths))

  # Calculate Mahalanobis distribution for each scan and identify outliers
  filtered.df <- FilterSpectra(df = input.df, filter = F, return.distances = T,
                               num.col.before.spectra = num.col.before.spectra,
                               window.size = 10) %>%
    mutate(Outlier = ifelse(.data$h.distances > chisq95, T, F))

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
                                   color = .data$Outlier)) + #TODO fix this unique id required
    geom_line(alpha = .5) +
    geom_line(data = subset(.data$hdists.df, .data$Outlier == T), alpha = .7) +
    scale_color_manual(values = wes_palette(name = "Zissou1")[c(1,5)], name = "Outlier?") +
    theme_minimal() +
    labs(title = paste0("Chi-Square (", length(wavelengths)," df) 95% cutoff for Mahalanobis distance"),
         subtitle = "Raw spectra", x = "Wavelength", y = "Spectral Value")

  # Print metadata for each outlier
  cat("Outliers:\n")
  print(filtered.df %>% dplyr::filter(.data$h.distances > chisq95) %>%
          dplyr::select(-starts_with("X")) %>% distinct())
  return(hdist.chisq95.plot)
}
