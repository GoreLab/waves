#' @title Plot spectral data, highlighting outliers as identified using
#'  Mahalanobis distance
#' @description Generates a \code{\link[ggplot2]{ggplot}} object of given
#'  spectra, with wavelength on the x axis and given spectral values on the y.
#'  Mahalanobis distance is used to calculate outliers, which are both
#'  identified on the plot. Rows from the original dataframe are printed to the
#'  console for each outlier that is identified.
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#'
#' @inheritParams filter_spectra
#' @param df \code{data.frame} object containing columns of spectra.
#'  Spectral columns must be labeled with an "X" and then the wavelength
#'  (example: "X740" = 740nm). Left-most column must be unique ID. May also
#'  contain columns of metadata between the unique ID and spectral columns.
#'  Cannot contain any missing values. Metadata column names may not start
#'  with "X".
#' @param num.col.before.spectra Number of columns to the left of the spectral
#'  matrix (including unique ID). Default is 1.
#' @param detect.outliers Boolean indicating whether spectra should be filtered
#'  before plotting. If \code{TRUE}, outliers are indicated by color in the
#'  resulting plot. If \code{verbose} is also set to \code{TRUE}, outlier
#'  metadata will be printed to the console. Default is \code{TRUE}.
#' @param color String or vector of strings indicating colors to be passed to
#'  \code{\link[ggplot2]{ggplot}}. Default is default
#'  \code{\link[ggplot2]{ggplot}} colors.
#' @param alternate.title String to be used as plot title. If
#'  \code{detect.outliers} is \code{TRUE}, a descriptive title will be supplied.
#'  If \code{detect.outliers} is \code{FALSE}, default is no title will be used.
#' @param wavelengths DEPRECATED \code{wavelengths} is no
#'  longer supported; this information is now inferred from
#'  \code{df} column names
#'
#' @importFrom dplyr select mutate distinct
#' @importFrom tidyselect starts_with
#' @importFrom readr parse_number
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs scale_color_manual
#' @importFrom scales hue_pal
#' @importFrom tidyr gather
#' @importFrom stringr str_extract
#' @importFrom rlang .data abort
#' @importFrom lifecycle deprecated
#'
#' @return If verbose, prints unique ID and metadata for rows identified as
#' outliers. Returns plot of spectral data with non-outliers in blue and
#' outliers in red. X-axis is wavelengths and y-axis is spectral values.
#' @export
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' ikeogu.2017 %>%
#'   dplyr::rename(unique.id = sample.id) %>%
#'   dplyr::select(unique.id, dplyr::everything(), -TCC) %>%
#'   na.omit() %>%
#'   plot_spectra(
#'     df = .,
#'     num.col.before.spectra = 5,
#'     window.size = 15,
#'     detect.outliers = TRUE,
#'     color = NULL,
#'     alternate.title = NULL,
#'     verbose = TRUE
#'   )
#' }
plot_spectra <- function(df,
                         num.col.before.spectra = 1,
                         window.size = 10,
                         detect.outliers = TRUE,
                         color = NULL,
                         alternate.title = NULL,
                         verbose = TRUE,
                         wavelengths = deprecated()) {

  # Strip off non-spectral columns
  spectra <- df %>%
    dplyr::select(tidyselect::starts_with("X"))

  # Error handling ---------------------------
  # mahalanobis function does not allow missing values or non-numeric data
  if (nrow(spectra) != nrow(na.omit(spectra))) {
    rlang::abort("Input data frame cannot contain missing values!
                 Remove them and try again.")
  }

  if (detect.outliers) { # Outlier detection

    # Color-related handling ---------------------------
    if (!is.null(color) && length(color) == 1) {
      rlang::abort("Two colors are required but only one was supplied.
                   Please add another value or set 'color' to 'NULL'")
    }

    if (!is.null(color) && length(color) > 2) {
      warning("Two colors are required but more than two were supplied.
              Only the first two will be used.")
      color <- color[1:2]
    }

    if (is.null(color)) {
      color <- scales::hue_pal()(2)
    }

    if (lifecycle::is_present(wavelengths)) {
      lifecycle::deprecate_warn(
        when = "0.2.0",
        what = "plot_spectra(wavelengths)",
        details = "Wavelength specification is now inferred from column names."
      )
    }

    wavelengths <- spectra %>%
      colnames() %>%
      readr::parse_number()

    # Plot title ---------------------------
    if (is.null(alternate.title)) {
      plot.title <- paste0("Chi-Square (", length(wavelengths),
                           " df) 95% cutoff for Mahalanobis distance")
    } else {
      plot.title <- alternate.title
    }

    # Calculate outlier cutoff ---------------------------
    chisq95 <- qchisq(.95, df = length(wavelengths))

    # Calculate Mahalanobis distribution for each scan and identify outliers ---
    filtered.df <- filter_spectra(
      df = df, filter = FALSE, return.distances = TRUE,
      num.col.before.spectra = num.col.before.spectra,
      window.size = window.size, verbose = FALSE
    ) %>%
      mutate(Outlier = ifelse(.data$h.distances > chisq95, TRUE, FALSE))

    # Prepare data frame for plotting ---------------------------
    hdists.df <- filtered.df %>%
      tibble::rownames_to_column(var = "rownames") %>%
      dplyr::select(.data$rownames, .data$h.distances, .data$Outlier,
                    tidyselect::starts_with("X")) %>%
      tidyr::gather(key = "wl", value = "s.value",
                    tidyselect::starts_with("X")) %>%
      dplyr::mutate(wl = as.numeric(readr::parse_number(.data$wl)))

    # Create plot ---------------------------
    spectral.plot <- ggplot2::ggplot(
      data = hdists.df,
      aes(
        x = .data$wl,
        y = .data$s.value,
        group = .data$rownames,
        color = .data$Outlier
      )
    ) +
      geom_line(alpha = .5) +
      geom_line(data = subset(hdists.df, hdists.df$Outlier == TRUE),
                alpha = .7) +
      scale_color_manual(values = color, name = "Outlier?") +
      theme_minimal() +
      labs(
        title = plot.title,
        x = "Wavelength",
        y = "Spectral Value"
      )

    # Print metadata for each outlier ---------------------------
    if (verbose) {
      if (sum(hdists.df$Outlier > 0)) {
        cat("Outliers:\n")
        print(
          filtered.df %>%
            dplyr::filter(.data$h.distances > chisq95) %>%
            dplyr::select(-tidyselect::starts_with("X")) %>%
            distinct()
        )
      } else {
        cat("No outliers detected.\n")
      }
    }
  } else { # No outlier detection

    # Color handling ---------------------------
    if (!is.null(color) && length(color) > 1) {
      warning("Only one color is required but more than one were supplied.
              Only the first value will be used")
      color <- color[1]
    }

    if (is.null(color)) {
      color <- scales::hue_pal()(1)
    }

    # Plot title
    if (!is.null(alternate.title)) {
      plot.title <- alternate.title
    }

    # Prepare data frame for plotting ---------------------------
    prepped.df <- df %>%
      tibble::rownames_to_column(var = "rownames") %>%
      dplyr::select(.data$rownames, tidyselect::starts_with("X")) %>%
      tidyr::gather(key = "wl", value = "s.value",
                    tidyselect::starts_with("X")) %>%
      dplyr::mutate(wl = as.numeric(stringr::str_extract(.data$wl,
                                                         "\\-*\\d+\\.*\\d*")))

    spectral.plot <- ggplot(
      data = prepped.df,
      aes(
        x = .data$wl,
        y = .data$s.value,
        group = .data$rownames
      )
    ) +
      geom_line(alpha = .5, color = color) +
      theme_minimal() +
      labs(title = plot.title, x = "Wavelength", y = "Spectral Value")
  }

  return(spectral.plot)
}
