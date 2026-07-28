library(waves)
library(magrittr)

# Shared test data
test_df <- ikeogu.2017 %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::rename(reference = DMC.oven, unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit()

# ── Seed forwarding ────────────────────────────────────────────────────────────

test_that("test_spectra is reproducible with the same seed", {
  skip_on_cran()
  r1 <- test_spectra(test_df, num.iterations = 3, tune.length = 3,
                     pretreatment = 1, seed = 42, verbose = FALSE)
  r2 <- test_spectra(test_df, num.iterations = 3, tune.length = 3,
                     pretreatment = 1, seed = 42, verbose = FALSE)
  expect_identical(r1$model.performance$RMSEp, r2$model.performance$RMSEp)
})

test_that("test_spectra gives different results for different seeds", {
  skip_on_cran()
  r1 <- test_spectra(test_df, num.iterations = 3, tune.length = 3,
                     pretreatment = 1, seed = 1, verbose = FALSE)
  r2 <- test_spectra(test_df, num.iterations = 3, tune.length = 3,
                     pretreatment = 1, seed = 2, verbose = FALSE)
  expect_false(identical(r1$model.performance$RMSEp, r2$model.performance$RMSEp))
})

# ── best.model.metric forwarding ───────────────────────────────────────────────

test_that("test_spectra accepts both valid best.model.metric values without error", {
  skip_on_cran()
  expect_no_error(
    test_spectra(test_df, num.iterations = 2, tune.length = 3,
                 pretreatment = 1, best.model.metric = "RMSE",
                 verbose = FALSE)
  )
  expect_no_error(
    test_spectra(test_df, num.iterations = 2, tune.length = 3,
                 pretreatment = 1, best.model.metric = "Rsquared",
                 verbose = FALSE)
  )
})

# ── SVM importance with multiple pretreatments ─────────────────────────────────

test_that("test_spectra svmLinear returns importance with multiple pretreatments", {
  skip_on_cran()
  skip_if_not_installed("kernlab")
  result <- test_spectra(test_df, num.iterations = 2, tune.length = 3,
                         pretreatment = 1:2, model.method = "svmLinear",
                         verbose = FALSE)
  expect_false(is.null(result$importance))
  expect_true(any(startsWith(names(result$importance), "X")))
})

# ── Output structure ───────────────────────────────────────────────────────────

test_that("test_spectra returns all expected output elements", {
  skip_on_cran()
  result <- test_spectra(test_df, num.iterations = 2, tune.length = 3,
                         pretreatment = 1, verbose = FALSE)
  expect_named(result, c("model", "summary.model.performance",
                         "model.performance", "predictions", "importance"))
})

test_that("test_spectra model.performance has correct number of rows", {
  skip_on_cran()
  n_iter <- 3
  result <- test_spectra(test_df, num.iterations = n_iter, tune.length = 3,
                         pretreatment = 1, verbose = FALSE)
  expect_equal(nrow(result$model.performance), n_iter)
})
