#' @title Internal utility functions
#' @name get_mode
#' @description Get the mode of a set of numbers. Used in getting summary of
#' results within [train_spectra()]
#'
#' @param vector.input The mode of this vector of numbers will be calculated
#' by this function
#' @return mode of the numbers in `vector.input`
#' @keywords internal
get_mode <- function(vector.input) {
  as.matrix(vector.input)
  unique.vector <- unique(vector.input)
  return(unique.vector[which.max(tabulate(match(vector.input,
                                                unique.vector)))])
}
