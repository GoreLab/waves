library(waves)
library(magrittr)

test_df <- ikeogu.2017 %>%
  dplyr::filter(study.name == "C16Mcal") %>%
  dplyr::rename(reference = DMC.oven, unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, dplyr::starts_with("X")) %>%
  na.omit()

# ── Seed forwarding ────────────────────────────────────────────────────────────

test_that("save_model forwards seed to test_spectra", {
  skip_on_cran()
  r1 <- save_model(test_df, write.model = FALSE, pretreatment = 1,
                   tune.length = 3, num.iterations = 3, seed = 1,
                   verbose = FALSE)
  r2 <- save_model(test_df, write.model = FALSE, pretreatment = 1,
                   tune.length = 3, num.iterations = 3, seed = 99999,
                   verbose = FALSE)
  expect_false(identical(r1$best.model.stats$RMSEp_mean,
                         r2$best.model.stats$RMSEp_mean))
})

test_that("save_model is reproducible with the same seed", {
  skip_on_cran()
  r1 <- save_model(test_df, write.model = FALSE, pretreatment = 1,
                   tune.length = 3, num.iterations = 3, seed = 42,
                   verbose = FALSE)
  r2 <- save_model(test_df, write.model = FALSE, pretreatment = 1,
                   tune.length = 3, num.iterations = 3, seed = 42,
                   verbose = FALSE)
  expect_identical(r1$best.model.stats$RMSEp_mean, r2$best.model.stats$RMSEp_mean)
})

# ── Output contract ────────────────────────────────────────────────────────────

test_that("save_model writes the file pair predict_spectra consumes", {
  skip_on_cran()
  tmp <- tempdir()
  save_model(test_df, write.model = TRUE, pretreatment = 1,
             model.save.folder = tmp, model.name = "sm_contract",
             tune.length = 3, num.iterations = 2, verbose = FALSE)
  expect_true(file.exists(file.path(tmp, "sm_contract.Rds")))
  expect_true(file.exists(file.path(tmp, "sm_contract_stats.csv")))
  # predict_spectra reads Pretreatment from the stats file
  stats <- utils::read.csv(file.path(tmp, "sm_contract_stats.csv"))
  expect_true("Pretreatment" %in% names(stats))
})

test_that("save_model selects a single best model across pretreatments", {
  skip_on_cran()
  result <- save_model(test_df, write.model = FALSE, pretreatment = 1:3,
                       tune.length = 3, num.iterations = 2, verbose = FALSE)
  expect_equal(nrow(result$best.model.stats), 1)
  expect_false(inherits(result$best.model, "list"))
})

test_that("save_model stats have the same shape for one or many pretreatments", {
  skip_on_cran()
  one <- save_model(test_df, write.model = FALSE, pretreatment = 1,
                    tune.length = 3, num.iterations = 2, verbose = FALSE)
  many <- save_model(test_df, write.model = FALSE, pretreatment = 1:3,
                     tune.length = 3, num.iterations = 2, verbose = FALSE)
  expect_equal(nrow(one$best.model.stats), 1)
  expect_equal(nrow(many$best.model.stats), 1)
  expect_identical(names(one$best.model.stats), names(many$best.model.stats))
})

# ── Deprecated arguments now error ─────────────────────────────────────────────

test_that("save_model deprecated arguments throw errors", {
  expect_error(
    save_model(test_df, write.model = FALSE, num.iterations = 2,
               save.model = TRUE),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    save_model(test_df, write.model = FALSE, num.iterations = 2,
               autoselect.preprocessing = TRUE),
    class = "lifecycle_error_deprecated"
  )
})
