#' @title Format multiple trials with or without overlapping genotypes into
#'   training and test sets according to user-provided cross validation scheme
#' @name format_cv
#' @author Jenna Hershberger \email{jmh579@@cornell.edu}
#' @description Standalone function that is also used within
#'   \code{\link{train_spectra}} to divide trials or studies into training and
#'   test sets based on overlap in trial environments and genotype entries
#' @details Use of a cross-validation scheme requires a column in the input
#'   \code{data.frame} named "genotype" to ensure proper sorting of training and
#'   test sets. Variables \code{trial1} and \code{trial2} are required, while
#'   \code{trial 3} is optional.
#'
#' @param trial1 \code{data.frame} object that is for use only when
#'   \code{cv.scheme} is provided. Contains the trial to be tested in subsequent
#'   model training functions. The first column contains unique identifiers,
#'   second contains genotypes, third contains reference values, followed by
#'   spectral columns. Include no other columns to right of spectra! Column
#'   names of spectra must start with "X", reference column must be named
#'   "reference", and genotype column must be named "genotype".
#' @param trial2 \code{data.frame} object that is for use only when
#'   \code{cv.scheme} is provided. This data.frame contains a trial that has
#'   overlapping genotypes with \code{trial1} but that were grown in a different
#'   site/year (different environment). Formatting must be consistent with
#'   \code{trial1}.
#' @param trial3 \code{data.frame} object that is for use only when
#'   \code{cv.scheme} is provided. This data.frame contains a trial that may or
#'   may not contain genotypes that overlap with \code{trial1}. Formatting must
#'   be consistent with \code{trial1}.
#' @param cv.scheme A cross validation (CV) scheme from Jarquín et al., 2017.
#'   Options for \code{cv.scheme} include:
#'   \itemize{
#'       \item "CV1": untested lines in tested environments
#'       \item "CV2": tested lines in tested environments
#'       \item "CV0": tested lines in untested environments
#'       \item "CV00": untested lines in untested environments
#'   }
#' @param stratified.sampling If \code{TRUE}, training and test
#'   sets will be selected using stratified random sampling. Default is
#'   \code{TRUE}.
#' @param proportion.train Fraction of samples to include in the training set.
#'   Default is 0.7.
#' @param seed Number used in the function \code{set.seed()} for reproducible
#'   randomization. If \code{NULL}, no seed is set. Default is \code{NULL}.
#' @param remove.genotype boolean that, if \code{TRUE}, removes the "genotype"
#'   column is removed from the output \code{data.frame}. Default is
#'   \code{FALSE}.
#'
#' @references Jarquín, D., C. Lemes da Silva, R. C. Gaynor, J. Poland, A.
#'   Fritz, R. Howard, S. Battenfield, and J. Crossa. 2017. Increasing
#'   genomic-enabled prediction accuracy by modeling genotype × environment
#'   interactions in Kansas wheat. Plant Genome 10(2):1-15.
#'   <doi:10.3835/plantgenome2016.12.0130>
#'
#' @importFrom dplyr group_by ungroup filter mutate rename select summarize
#' @importFrom tidyr nest unnest drop_na
#' @importFrom magrittr %>%
#' @importFrom rlang .data abort
#' @importFrom caret createDataPartition createResample
#' @importFrom tidyselect starts_with
#'
#' @return List of data.frames ($train.set, $test.set) compiled according to
#'   user-provided cross validation scheme.
#' @export
#'
#' @examples
#' # Must have a column called "genotype", so we'll create a fake one for now
#' # We will use CV00, which does not require any overlap in genotypes
#' # In real scenarios, CV schemes that rely on genotypes should not be applied
#' # when genotypes are unknown, as in this case.
#' library(magrittr)
#' trials <- ikeogu.2017 %>%
#'   dplyr::mutate(genotype = 1:nrow(ikeogu.2017)) %>% # fake for this example
#'   dplyr::rename(reference = DMC.oven) %>%
#'   dplyr::select(
#'     study.name, sample.id, genotype, reference,
#'     tidyselect::starts_with("X")
#'   )
#' trial1 <- trials %>%
#'   dplyr::filter(study.name == "C16Mcal") %>%
#'   dplyr::select(-study.name)
#' trial2 <- trials %>%
#'   dplyr::filter(study.name == "C16Mval") %>%
#'   dplyr::select(-study.name)
#' cv.list <- format_cv(
#'   trial1 = trial1, trial2 = trial2, cv.scheme = "CV00",
#'   stratified.sampling = FALSE, remove.genotype = TRUE
#' )
#' cv.list$train.set[1:5, 1:5]
#' cv.list$test.set[1:5, 1:5]
format_cv <- function(trial1,
                      trial2,
                      trial3 = NULL,
                      cv.scheme,
                      stratified.sampling = TRUE,
                      proportion.train = 0.7,
                      seed = NULL,
                      remove.genotype = FALSE) {
  # Error handling
  if (!cv.scheme %in% c("CV0", "CV00", "CV1", "CV2")) {
    rlang::abort("cv.scheme must be 'CV0', 'CV00', 'CV1', or 'CV2'")
  }

  if (!"genotype" %in% colnames(trial1) || !"genotype" %in% colnames(trial2)) {
    rlang::abort("trial1 and trial2 must each have a column named 'genotype'")
  }

  if (proportion.train > 1 || proportion.train < 0) {
    rlang::abort("'proportion.train' must be a number between 0 and 1")
  }

  overlapping.genos <- trial1[which(trial1$genotype %in%
                                      rbind(trial2, trial3)[, "genotype"]), ]
  if ((cv.scheme %in% c("CV2", "CV0")) && (nrow(overlapping.genos) < 1)) {
    rlang::abort("There are no overlapping genotypes between the trials
  provided,so the CV scheme you have chosen cannot be used.
  Please choose another, more appropriate CV scheme (CV1 or CV00).")
  }


  if (!is.null(seed)) {
    set.seed(seed)
  }

  t1 <- trial1 %>%
    dplyr::group_by(.data$genotype) %>%
    tidyr::nest(data = c(-.data$genotype))

  # Random sampling
  if (!stratified.sampling) {
    train.index <- sort(sample(
      x = seq_len(nrow(t1)),
      size = proportion.train * nrow(t1),
      replace = FALSE, prob = NULL
    ))
  } else if (stratified.sampling) {
    # Stratified sampling based on genotype mean of reference values
    t1_summary <- trial1 %>%
      dplyr::group_by(.data$genotype) %>%
      dplyr::summarize(reference.mean = mean(.data$reference))
    train.index <- caret::createDataPartition(
      y = t1_summary$reference.mean,
      p = proportion.train
    ) %>%
      unlist()
  }

  # t1.a is always the test set
  t1.a <- t1[-train.index, ] %>%
    tidyr::unnest(c(-.data$genotype)) %>%
    dplyr::ungroup()
  t1.b <- t1[train.index, ] %>%
    tidyr::unnest(c(-.data$genotype)) %>%
    dplyr::ungroup()
  # we want t2.a to be the same genotypes as in t1.a
  # and t2.b to be the same genotypes as t1.b
  t2.a <- trial2[which(trial2$genotype %in% t1.a$genotype), ]
  t2.b <- trial2[which(trial2$genotype %in% t1.b$genotype), ]

  if (cv.scheme == "CV0") {
    # Tested lines in untested environment
    test.set <- t1.a
    train.set <- rbind(trial2, trial3)
  }

  if (cv.scheme == "CV00") {
    # Untested lines in untested environment
    # check for overlapping genotypes and remove
    # from either training or test set
    trial2.no.overlap <- trial2 %>%
      dplyr::filter(!.data$genotype %in% t1.a$genotype)
    if (!is.null(trial3)) {
      trial3.no.overlap <- trial3 %>%
        dplyr::filter(!.data$genotype %in% t1.a$genotype)
    } else {
      trial3.no.overlap <- NULL
    }
    test.set <- t1.a
    train.set <- rbind(trial2.no.overlap, trial3.no.overlap)
  }

  if (cv.scheme == "CV1") {
    # Untested lines in tested environment
    test.set <- t1.a
    train.set <- rbind(t1.b, t2.b)
  }

  if (cv.scheme == "CV2") {
    # Tested lines in tested environment
    test.set <- t1.a
    train.set <- rbind(t1.b, trial2)
  }

  if (remove.genotype) {
    train.set <- train.set %>%
      dplyr::select(-.data$genotype) %>%
      tidyr::drop_na()
    test.set <- test.set %>%
      dplyr::select(-.data$genotype) %>%
      tidyr::drop_na()
  }

  return(list(
    train.set = train.set,
    test.set = test.set
  ))
}
