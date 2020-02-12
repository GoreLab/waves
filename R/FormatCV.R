#' @title Format multiple trials with or without overlapping genotypes into training and test sets
#' according to user-provided cross validation scheme
#' @name FormatCV
#' @description Standalone function that is also used within [TrainSpectralModel()]
#' @details Use of a cross-validation scheme requires a column in the input `data.frame` named
#' "genotype" to ensure proper sorting of training and test sets. Variables `trial1` and `trial2`
#' are required, while `trial 3` is optional.
#'
#' @param trial1 `data.frame` object that is for use only when `cv.scheme` is provided.
#' Contains the trial to be tested in subsequent model training functions. The first column
#' contains unique identifiers, second contains genotypes, third contains reference values,
#' followed by spectral columns. Include no other columns to right of spectra! Column names
#' of spectra must start with "X", reference column must be named "reference", and genotype column
#' must be named "genotype".
#' @param trial2 `data.frame` object that is for use only when `cv.scheme` is provided.
#' This data.frame contains a trial that has overlapping genotypes with `trial1`
#' but that were grown in a different site/year (different environment). Formatting must be
#' consistent with `trial1`.
#' @param trial3 `data.frame` object that is for use only when `cv.scheme` is provided.
#' This data.frame contains a trial that may or may not contain genotypes that overlap with `trial1`.
#' Formatting must be consistent with `trial1`.
#' @param cv.scheme A cross validation (CV) scheme from Jarquín et al., 2017.
#' Options for cv.scheme include:
#' *"CV1": untested lines in tested environments
#' *"CV2": tested lines in tested environments
#' *"CV0": tested lines in untested environments
#' *"CV00": untested lines in untested environments
#' @param seed Number used in the function `set.seed()` for reproducible randomization.
#' If `NULL`, no seed is set. Default is `NULL`.
#' @param remove.genotype boolean that, if `TRUE`, removes the "genotype" column is removed from
#' the output `data.frame`. Default is `FALSE`.
#'
#' @references Jarquín, D., C. Lemes da Silva, R. C. Gaynor, J. Poland, A. Fritz, R. Howard,
#' S. Battenfield, and J. Crossa. 2017. Increasing Genomic-Enabled Prediction Accuracy by Modeling
#' Genotype × Environment Interactions in Kansas Wheat. Plant Genome 10.2
#' doi:10.3835/plantgenome2016.12.0130
#'
#' @importFrom dplyr group_by ungroup filter
#' @importFrom tidyr nest unnest
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return List of data.frames (training set, test set) compiled according to user-provided
#' cross validation scheme.
#' @export
#'
#' @examples
FormatCV <- function(trial1,
                     trial2,
                     trial3,
                     cv.scheme,
                     seed = NULL,
                     remove.genotype = FALSE){
  # Error handling
  if(!cv.scheme %in% c("CV0", "CV00", "CV1", "CV2")){
    stop("cv.scheme must be 'CV0', 'CV00', 'CV1', or 'CV2'")
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  t1 <- trial1 %>% dplyr::group_by(.data$genotype) %>% tidyr::nest(data = c(-.data$genotype))
  t2 <- trial2 %>% dplyr::group_by(.data$genotype) %>% tidyr::nest(data = c(-.data$genotype))
  # Random sampling
  train.index <- sort(sample(x = seq(from = 1, to = nrow(t1), by = 1),
                             size = 0.7 * nrow(t1),
                             replace = F, prob = NULL))
  # t1.a is always the test set
  t1.a <- t1[-train.index,] %>% tidyr::unnest(c(-.data$genotype)) %>% dplyr::ungroup()
  t1.b <- t1[train.index,] %>% tidyr::unnest(c(-.data$genotype)) %>% dplyr::ungroup()
  # we want t2.a to be the same genotypes as in t1.a and t2.b to be same as t1.b
  t2.a <- t2[which(t2$genotype %in% t1.a$genotype),] %>%
    tidyr::unnest(c(-.data$genotype)) %>% dplyr::ungroup()
  t2.b <- t2[which(t2$genotype %in% t1.b$genotype),] %>%
    tidyr::unnest(c(-.data$genotype)) %>% dplyr::ungroup()

  if(cv.scheme == "CV0"){
    # Tested lines in untested environment
    test.set <- t1.a
    train.set <- rbind(trial2, trial3)
  }

  if(cv.scheme == "CV00"){
    # Untested lines in untested environment
    # check for overlapping genotypes and remove from either training or test set
    trial2.no.overlap <- trial2 %>% dplyr::filter(!.data$genotype %in% t1.a$genotype)
    trial3.no.overlap <- trial3 %>% dplyr::filter(!.data$genotype %in% t1.a$genotype)
    test.set <- t1.a
    train.set <- rbind(trial2.no.overlap, trial3.no.overlap)
  }

  if(cv.scheme == "CV1"){
    # Untested lines in tested environment
    test.set <- t1.a
    train.set <- rbind(t1.b, t2.b)
  }

  if(cv.scheme == "CV2"){
    # Tested lines in tested environment
    test.set <- t1.a
    train.set <- rbind(t1.b, trial2)
  }

  if(remove.genotype){
    train.set <- train.set %>% dplyr::select(-.data$genotype)
    test.set <- test.set %>% dplyr::select(-.data$genotype)
  }

  return(list(train.set, test.set))
}
