% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_spectra.R
\name{predict_spectra}
\alias{predict_spectra}
\title{Use provided model object to predict trait values with input dataset}
\usage{
predict_spectra(
  input.data,
  model.stats.location,
  model.location,
  model.method = "pls",
  wavelengths = deprecated()
)
}
\arguments{
\item{input.data}{\code{data.frame} object of spectral data for input into a
spectral prediction model. First column contains unique identifiers
followed by spectral columns. Include no other columns to right of spectra!
Column names of spectra must start with "X".}

\item{model.stats.location}{String containing file path (including file name)
to save location of "(model.name)_stats.csv" as output from the
\code{\link{save_model}} function.}

\item{model.location}{String containing file path (including file name) to
location where the trained model ("(model.name).Rds") was saved as output
by the \code{\link{save_model}} function.}

\item{model.method}{Model type to use for training. Valid options include:
\itemize{ \item "pls": Partial least squares regression (Default) \item
"rf": Random forest \item "svmLinear": Support vector machine with linear
kernel \item "svmRadial": Support vector machine with radial kernel }}

\item{wavelengths}{DEPRECATED \code{wavelengths} is no
longer supported; this information is now inferred from \code{input.data}
column names}
}
\value{
\code{data.frame} object of predictions for each sample (row). First
  column is unique identifier supplied by \code{input.data} and second is
  predicted values
}
\description{
Loads an existing model and cross-validation performance
  statistics (created with \code{\link{save_model}}) and makes predictions
  based on new spectra.
}
\examples{
\dontrun{
ikeogu.2017 \%>\%
  dplyr::select(sample.id, dplyr::starts_with("X")) \%>\%
  predict_spectra(
    input.data = .,
    model.stats.location = paste0(
      getwd(),
      "/my_model_stats.csv"
    ),
    model.location = paste0(getwd(), "/my_model.Rds")
  )
}

}
\author{
Jenna Hershberger \email{jmh579@cornell.edu}
}
