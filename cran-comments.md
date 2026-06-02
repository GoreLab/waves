## Test environments
* local Apple Silicon (M1), macOS 26.5.1 Tahoe, R 4.6.0
* GitHub Actions CI:
  - ubuntu-24.04 (release, devel)
  - windows-latest (release)
  - macOS-latest (release)
* R-hub
  - linux (R-devel)
  - macos (R-devel)
  - windows (R-devel)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Comments
This is a maintenance release that fixes several bugs in `train_spectra()`,
`test_spectra()`, `predict_spectra()`, and internal utility functions:

* The final RF model was trained with `ntree = tune.length` (≤5 trees) instead
  of the standard 500; the tuned `mtry` was also not applied to the final model
* The final SVM model incorrectly included `unique.id` as a predictor
* The final PLS model used `ncomp = tune.length` instead of the best value
  identified during training iterations; `predict_spectra()` now reads `ncomp`
  directly from the model object
* `train_spectra()` crashed with `undefined columns selected` when `cv.scheme`
  was used, due to column indices computed before `format_cv()` removed the
  genotype column
* When `cv.scheme` is used, the final model was trained on incorrect data
  (either all trials combined or trial1 alone); it now calls `format_cv()` to
  use the scheme-appropriate training set
* `set.seed()` was called after `createDataPartition()`, making stratified
  train/test splits non-reproducible even when a seed was set
* `best.model.metric` and `seed` were accepted by `test_spectra()` but never
  forwarded to `train_spectra()`, silently ignoring user input
* SVM importance output was corrupted (1×1 matrix instead of NULL) when
  running with multiple pretreatments
* Invalid `ntree` and `mtry` arguments were passed to `predict.randomForest()`,
  where they are silently ignored

All existing user-facing APIs remain fully compatible.

## Downstream dependencies
There are currently no downstream dependencies for this package.
