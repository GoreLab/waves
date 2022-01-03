#' @title Aggregate data based on grouping variables and a user-provided
#'   function
#' @name aggregate_spectra
#' @description Use grouping variables to collapse spectral \code{data.frame} by
#'   mean or median. Recommended for use after \code{\link{filter_spectra}}
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#' @importFrom stats aggregate median
#' @importFrom dplyr select group_by_at summarize across
#' @importFrom magrittr %>%
#' @importFrom tidyselect starts_with all_of everything
#' @usage aggregate_spectra(df, grouping.colnames, reference.value.colname,
#'   agg.function)
#'
#' @param df \code{data.frame} object containing one or multiple columns of
#'   grouping variables (must be consistent within each group), column of
#'   reference values (optional), and columns of spectra. Spectral column names
#'   must start with "X".
#' @param grouping.colnames Names of columns to be used as grouping variables.
#'   Minimum 2 variables required. Default is c("trial", "plot").
#' @param reference.value.colname Name of reference column to be aggregated
#'   along with spectra. Default is "reference"
#' @param agg.function Name of function (string format) to be used for sample
#'   aggregation. Must be either "mean" or "median". Default is "mean".
#'
#' @return \code{data.frame} object \code{df} aggregated based on grouping
#'   column by \code{agg.function}
#' @export
#'
#' @examples
#' library(magrittr)
#' aggregated.test <- ikeogu.2017 %>%
#'   dplyr::select(-TCC) %>%
#'   na.omit() %>%
#'   aggregate_spectra(
#'     grouping.colnames = c("study.name"),
#'     reference.value.colname = "DMC.oven",
#'     agg.function = "mean"
#'   )
#' aggregated.test[1:5, 1:5]
aggregate_spectra <- function(df,
                              grouping.colnames = c("unique.id"),
                              reference.value.colname = "reference",
                              agg.function = "mean") {
  # Error handling
  if (!(agg.function %in% c("mean", "median"))) {
    rlang::abort('agg.function must be either "mean" or "median"')
  }

  if (nrow(df) != nrow(na.omit(df))) {
    rlang::abort("df cannot contain missing values. Omit rows with missing values and try again.")
  }

  # Set aggregation function to match input
  if (agg.function == "median") {
    agg.function <-
      function(x) {
        suppressWarnings(median(as.numeric(as.character(x))))
      }
  } else {
    agg.function <-
      function(x) {
        suppressWarnings(mean(as.numeric(as.character(x)), na.rm = T))
      }
  }

  # Aggregate data.frame
  df.aggregated <- df %>%
    dplyr::select(
      tidyselect::all_of(grouping.colnames),
      tidyselect::all_of(reference.value.colname),
      tidyselect::starts_with("X")
    ) %>%
    dplyr::group_by_at(grouping.colnames) %>%
    dplyr::summarize(dplyr::across(.cols = tidyselect::everything(),
                                   .fns = agg.function))

  return(df.aggregated)
}
