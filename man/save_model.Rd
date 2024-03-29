% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_model.R
\name{save_model}
\alias{save_model}
\title{Save spectral prediction model and model performance statistics}
\usage{
save_model(
  df,
  write.model = TRUE,
  pretreatment = 1,
  model.save.folder = NULL,
  model.name = "PredictionModel",
  best.model.metric = "RMSE",
  k.folds = 5,
  proportion.train = 0.7,
  tune.length = 50,
  model.method = "pls",
  num.iterations = 10,
  stratified.sampling = TRUE,
  cv.scheme = NULL,
  trial1 = NULL,
  trial2 = NULL,
  trial3 = NULL,
  seed = 1,
  verbose = TRUE,
  save.model = deprecated(),
  wavelengths = deprecated(),
  autoselect.preprocessing = deprecated(),
  preprocessing.method = deprecated()
)
}
\arguments{
\item{df}{\code{data.frame} object. First column contains unique identifiers,
second contains reference values, followed by spectral columns. Include no
other columns to right of spectra! Column names of spectra must start with
"X" and reference column must be named "reference"}

\item{write.model}{If \code{TRUE}, the trained model will be saved in .Rds
format to the location specified by \code{model.save.folder}. If
\code{FALSE}, the best model will be output by the function but will not
save to a file. Default is \code{TRUE}.}

\item{pretreatment}{Number or list of numbers 1:13 corresponding to
  desired pretreatment method(s):
\enumerate{
  \item Raw data (default)
  \item Standard normal variate (SNV)
  \item SNV and first derivative
  \item SNV and second derivative
  \item First derivative
  \item Second derivative
  \item Savitzky–Golay filter (SG)
  \item SNV and SG
  \item Gap-segment derivative (window size = 11)
  \item SG and first derivative (window size = 5)
  \item SG and first derivative (window size = 11)
  \item SG and second derivative (window size = 5)
  \item SG and second derivative (window size = 11)
}}

\item{model.save.folder}{Path to folder where model will be saved. If not
provided, will save to working directory.}

\item{model.name}{Name that model will be saved as in
\code{model.save.folder}. Default is "PredictionModel".}

\item{best.model.metric}{Metric used to decide which model is best. Must be
either "RMSE" or "Rsquared"}

\item{k.folds}{Number indicating the number of folds for k-fold
cross-validation during model training. Default is 5.}

\item{proportion.train}{Fraction of samples to include in the training set.
Default is 0.7.}

\item{tune.length}{Number delineating search space for tuning of the PLSR
hyperparameter \code{ncomp}. Must be set to 5 when using the random forest
algorithm (\code{model.method == rf}). Default is 50.}

\item{model.method}{Model type to use for training. Valid options include:
\itemize{ \item "pls": Partial least squares regression (Default) \item
"rf": Random forest \item "svmLinear": Support vector machine with linear
kernel \item "svmRadial": Support vector machine with radial kernel }}

\item{num.iterations}{Number of training iterations to perform}

\item{stratified.sampling}{If \code{TRUE}, training and test sets will be
selected using stratified random sampling. This term is only used if
\code{test.data == NULL}. Default is \code{TRUE}.}

\item{cv.scheme}{A cross validation (CV) scheme from Jarquín et al., 2017.
Options for \code{cv.scheme} include:
\itemize{
    \item "CV1": untested lines in tested environments
    \item "CV2": tested lines in tested environments
    \item "CV0": tested lines in untested environments
    \item "CV00": untested lines in untested environments
}}

\item{trial1}{\code{data.frame} object that is for use only when
\code{cv.scheme} is provided. Contains the trial to be tested in subsequent
model training functions. The first column contains unique identifiers,
second contains genotypes, third contains reference values, followed by
spectral columns. Include no other columns to right of spectra! Column
names of spectra must start with "X", reference column must be named
"reference", and genotype column must be named "genotype".}

\item{trial2}{\code{data.frame} object that is for use only when
\code{cv.scheme} is provided. This data.frame contains a trial that has
overlapping genotypes with \code{trial1} but that were grown in a different
site/year (different environment). Formatting must be consistent with
\code{trial1}.}

\item{trial3}{\code{data.frame} object that is for use only when
\code{cv.scheme} is provided. This data.frame contains a trial that may or
may not contain genotypes that overlap with \code{trial1}. Formatting must
be consistent with \code{trial1}.}

\item{seed}{Integer to be used internally as input for \code{set.seed()}.
Only used if \code{stratified.sampling = TRUE}. In all other cases, seed
is set to the current iteration number. Default is 1.}

\item{verbose}{If \code{TRUE}, the number of rows removed through filtering
will be printed to the console. Default is \code{TRUE}.}

\item{save.model}{DEPRECATED \code{save.model = FALSE} is no
longer supported; this function will always return a saved model.}

\item{wavelengths}{DEPRECATED \code{wavelengths} is no
longer supported; this information is now inferred from \code{df}
column names}

\item{autoselect.preprocessing}{DEPRECATED
\code{autoselect.preprocessing = FALSE} is no longer supported. If
multiple pretreatment methods are supplied, the best will be automatically
selected as the model to be saved.}

\item{preprocessing.method}{DEPRECATED \code{preprocessing.method}
has been renamed "pretreatment"}
}
\value{
List of model stats (in \code{data.frame}) and trained model object.
  If the parameter \code{write.model} is TRUE, both objects are saved to
  \code{model.save.folder}. To use the optimally trained model for
  predictions, use tuned parameters from \code{$bestTune}.
}
\description{
Given a set of pretreatment methods, saves the best spectral
prediction model and model statistics to \code{model.save.folder} as
\code{model.name.Rds} and \code{model.name_stats.csv} respectively. If only
one pretreatment method is supplied, results from that method are stored.
}
\details{
Wrapper that uses \code{\link{pretreat_spectra}},
  \code{\link{format_cv}}, and \code{\link{train_spectra}} functions.
}
\examples{
\donttest{
library(magrittr)
test.model <- ikeogu.2017 \%>\%
  dplyr::filter(study.name == "C16Mcal") \%>\%
  dplyr::rename(reference = DMC.oven,
                unique.id = sample.id) \%>\%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) \%>\%
  na.omit() \%>\%
  save_model(
    df = .,
    write.model = FALSE,
    pretreatment = 1:13,
    model.name = "my_prediction_model",
    tune.length = 3,
    num.iterations = 3
  )
summary(test.model$best.model)
test.model$best.model.stats
}
}
\author{
Jenna Hershberger \email{jmh579@cornell.edu}
}
