#' @title Filter spectral data frame based on Mahalanobis distance
#' @name FilterSpectra
#' @description Determine Mahalanobis distances of observations (rows) within a given `data.frame`
#' with spectral data. Option to filter out observations based on these distances.
#' @details  This function uses a chi-square distribution with 95% cutoff where degrees of
#' freedom = number of wavelengths (columns) in the input `data.frame`.
#' @references Johnson, R.A., and D.W. Wichern. 2007. Applied Multivariate Statistical Analysis (6th Edition). pg 189
#' @author Jenna Hershberger \url{jmh579@@cornell.edu}
#' @usage FilterSpectra(df, filter, return.distances, num.col.before.spectra, window.size)
#' @importFrom stats cov mahalanobis na.omit qchisq
#' @param df obj a \code{data.frame} object containing columns of spectra and rows of observations. May also contain columns of metadata to the left of the spectra.
#' @param filter boolean that determines whether or not the input \code{data.frame} will be filtered.
#' If \code{TRUE}, \code{df} will be filtered according to squared Mahalanobis distance with a 95% cutoff from a chi-square
#' distribution with degrees of freedom = number of spectral columns. If \code{FALSE}, a column of squared Mahalanobis
#' distances \code{h.distance} will be added to the right side of df and all rows will be returned. Default is `TRUE`.
#' @param return.distances boolean that determines whether a column of squared Mahalanobis distances will be included
#' in output \code{data.frame}. If \code{TRUE}, a column of Mahalanobis distances for each row will be added to the right
#' side of \code{df}. Default is \code{FALSE}.
#' @param num.col.before.spectra number of columns to the left of the spectral matrix in \code{df}. Default is 4.
#' @param window.size number defining the size of window to use when calculating the covariance of the
#' spectra (required to calculate Mahalanobis distance). Default is 10.
#'
#' @return If 'filter' is TRUE, returns filtered data frame 'df' and reports the number of rows removed.
#'       The Mahalanobis distance with a cutoff of 95% of chi-square distribution
#'       (degrees of freedom = number of wavelengths)is used as filtering criteria.
#'       If 'filter' is FALSE, returns full input df with column 'h.distances' containing the Mahalanobis
#'       distance for each row.
#' @export
#'
#' @examples
#' \dontrun{
#' ikeogu.2017 %>%
#'   dplyr::select(-TCC) %>%
#'   na.omit() %>%
#'   FilterSpectra(df = .,
#'                 filter = T,
#'                 return.distances = F,
#'                 num.col.before.spectra = 5)
#' }
FilterSpectra <- function(df,
                          filter = T,
                          return.distances = F,
                          num.col.before.spectra = 4,
                          window.size = 10
                          ){

  # Error handling
  # mahalanobis function does not allow missing values or non-numeric data
  if(nrow(df) != nrow(na.omit(df))){
    stop("Input data frame cannot contain missing values! Remove them and try again.")
  }
  # Remove missing values (line above should stop them but this is just in case)
  df <- na.omit(df)

  # Strip off non-spectral columns
  spectra <- df[, (num.col.before.spectra + 1):ncol(df)]

  # Make subset of spectra using provided window size (otherwise the system is computationally singular)
  spectra.subset <- spectra[, seq(1, ncol(spectra), window.size)]

  # Calculate covariance of spectral matrix
  spectra.cov <- cov(as.matrix(spectra.subset))

  # Create list of Mahalanobis distances for each sample and bind to input df
  h.distances <- mahalanobis(x = spectra.subset, center = colMeans(spectra.subset),
                             cov = spectra.cov, tol=1e-22)
  if(sum(h.distances <= 0) > 0){
    stop("Please increase window size.")
  }
  df.distances <- cbind(df, h.distances)

  if(filter){
    # Filter input data based on square of Mahalanobis distance
    chisq95 <- qchisq(.95, df = ncol(spectra))
    df.filtered <- df.distances[which(h.distances < chisq95),]

    # How many samples were removed?
    if( nrow(df) - nrow(df.filtered) != 1){
      cat(paste("\nRemoved", nrow(df) - nrow(df.filtered), "rows.\n"))
    } else{
      cat(paste("\nRemoved 1 row.\n"))
    }

    if(return.distances){
      return(df.filtered)
    } else{
      return(dplyr::select(df.filtered, -h.distances))
    }

  } else{
    # Don't filter, just return input df with or without distances as rightmost column
    if(return.distances){
      return(df.distances)
    } else{
      return(dplyr::select(df.distances, -h.distances))
    }
  }
}
