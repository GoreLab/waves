#' @title Aggregate data based on grouping variables and a user-provided function
#' @name AggregateSpectra
#' @description Use grouping variables to collapse spectral `data.frame` by mean or median.
#' Recommended for use after `FilterSpectra`
#' @author Jenna Hershberger
#' @importFrom stats aggregate median
#' @importFrom magrittr %>%
#' @importFrom tidyselect starts_with
#' @usage AggregateSpectra(df, grouping.colnames, reference.value.colname, agg.function)
#'
#' @param df `data.frame` object containing one or multiple columns of grouping variables
#' (must be consistant for each group), column of reference values (optional),
#' and columns of spectra. Spectral column names must start with "X".
#' @param grouping.colnames Names of columns to be used as grouping variables.
#' Minimum 2 variables required. Default is c("trial", "plot").
#' @param reference.value.colname Name of reference column to be aggregated along with spectra. Default is "reference"
#' @param agg.function Name of function (string format) to be used for sample aggregation.
#' Must be either "mean" or "median". Default is "mean".
#'
#' @return `data.frame` object `df` aggregated based on grouping column by `agg.function`
#' @export
#'
#' @examples
AggregateSpectra <- function(df,
                             grouping.colnames = c("trial", "plot"),
                             reference.value.colname = "reference",
                             agg.function = "mean"){
  # Error handling
  if(!(agg.function %in% c("mean", "median"))){
    stop('agg.function must be either "mean" or "median"')
  }

  if(nrow(df) != nrow(na.omit(df))){
    stop("df cannot contain missing values. Omit rows with missing values and try again.")
  }

  # Set aggregation function to match input
  if(agg.function == "median"){
    agg.function <- function(x) median(as.numeric(as.character(x)))
  } else{
    agg.function <- function(x) mean(as.numeric(as.character(x)), na.rm= T)
  }

  # Aggregate data.frame
  df.aggregated <- df %>%
    dplyr::select(grouping.colnames, reference.value.colname, tidyselect::starts_with("X")) %>%
    aggregate(by = df[,grouping.colnames], FUN = agg.function)
  # remove duplicated columns
  cols.to.remove <- (length(grouping.colnames)+1):((length(grouping.colnames))+length(grouping.colnames))
  df.aggregated <- df.aggregated[,-cols.to.remove]

  return(df.aggregated)
}
