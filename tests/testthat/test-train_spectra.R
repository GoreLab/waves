library(waves)
library(magrittr)

# Shared test data - small subset for speed
test_df <- ikeogu.2017 %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::rename(reference = DMC.oven, unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit()

# ── Seed reproducibility ───────────────────────────────────────────────────────

test_that("train_spectra is reproducible with the same seed", {
  skip_on_cran()
  r1 <- train_spectra(test_df, num.iterations = 3, tune.length = 3,
                      seed = 42, verbose = FALSE)
  r2 <- train_spectra(test_df, num.iterations = 3, tune.length = 3,
                      seed = 42, verbose = FALSE)
  expect_identical(r1$model.performance$RMSEp, r2$model.performance$RMSEp)
})

test_that("train_spectra gives different results for different seeds", {
  skip_on_cran()
  r1 <- train_spectra(test_df, num.iterations = 3, tune.length = 3,
                      seed = 1, verbose = FALSE)
  r2 <- train_spectra(test_df, num.iterations = 3, tune.length = 3,
                      seed = 2, verbose = FALSE)
  expect_false(identical(r1$model.performance$RMSEp, r2$model.performance$RMSEp))
})

# ── PLS final model ────────────────────────────────────────────────────────────

test_that("train_spectra PLS final model uses modal best ncomp from iterations", {
  skip_on_cran()
  result <- train_spectra(test_df, num.iterations = 3, tune.length = 5,
                          model.method = "pls", verbose = FALSE)
  modal_ncomp <- waves:::get_mode(result$model.performance$best.ncomp)
  expect_equal(result$model$ncomp, modal_ncomp)
})

# ── RF final model ─────────────────────────────────────────────────────────────

test_that("train_spectra RF final model has 500 trees", {
  skip_on_cran()
  result <- train_spectra(test_df, num.iterations = 3, tune.length = 5,
                          model.method = "rf", verbose = FALSE)
  expect_equal(result$model$ntree, 500)
})

test_that("train_spectra RF final model uses modal best mtry from iterations", {
  skip_on_cran()
  result <- train_spectra(test_df, num.iterations = 3, tune.length = 5,
                          model.method = "rf", verbose = FALSE)
  modal_mtry <- waves:::get_mode(result$model.performance$best.mtry)
  expect_equal(result$model$mtry, modal_mtry)
})

# ── SVM final model ────────────────────────────────────────────────────────────

test_that("train_spectra SVM final model does not include unique.id as predictor", {
  skip_on_cran()
  skip_if_not_installed("kernlab")
  result <- train_spectra(test_df, num.iterations = 3, tune.length = 3,
                          model.method = "svmLinear", verbose = FALSE)
  expect_false("unique.id" %in% result$model$coefnames)
  expect_equal(length(result$model$coefnames), sum(startsWith(names(test_df), "X")))
})
