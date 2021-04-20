# waves

<!-- badges: start -->
![language: R](https://img.shields.io/badge/language-R-blue.svg)
![CRAN/METACRAN](https://img.shields.io/cran/v/waves?label=CRAN)
[![R build status](https://github.com/GoreLab/waves/workflows/R-CMD-check/badge.svg)](https://github.com/GoreLab/waves/actions)
<!-- badges: end -->

Originally designed application in the context of resource-limited plant research and breeding programs, `waves` provides an open-source solution to spectral data processing and model development by bringing useful packages together into a streamlined pipeline. This package is wrapper for functions related to the analysis of point visible and near-infrared reflectance measurements. It includes visualization, filtering, aggregation, preprocessing, cross-validation set formation, model training, and prediction functions to enable open-source association of spectral and reference data. 

## Cite
This package is documented in a peer-reviewed manuscript in the Plant Phenome Journal. Please cite the manuscript if you have found this package to be useful! 

> Hershberger, J, Morales, N, Simoes, CC, Ellerbrock, B, Bauchet, G, Mueller, LA, Gore MA. Making waves in Breedbase: An integrated spectral data storage and analysis pipeline for plant breeding programs. Plant Phenome J. 2021; 4:e20012. https://doi.org/10.1002/ppj2.20012


## Use

Follow the installation instructions below, and then go wild! Use `waves` to analyze your own data. Please report any bugs or feature requests by opening issues in this repository.

## Installation
Install the latest `waves` release directly from CRAN: 
``` r
install.packages("waves")
```
Alternatively, install the development version to get the most up-to-date (but not necessarily thoroughly tested) version:
``` r
# install.packages("devtools")
devtools::install_github("GoreLab/waves")
```

## Overview
1. Format your data. Match spectra with reference values so that you have a dataframe with unique identifiers, reference values, and other metadata as columns to the left of spectral values. Spectral column names should start with "X".
![Example Format](./man/figures/formatted_data.png)

2. Visualize and filter spectra using `PlotSpectra()` and `FilterSpectra()`.
![Filter data](./man/figures/filter_data.png)

3. If you have more than one scan per unique identifier, aggregate the scans by mean or median with `AggregateSpectra()`.
![Aggregate](./man/figures/aggregate.png)

4. Use `TestModelPerformance()` to perform preprocessing, cross-validation set formation, and model training functions over multiple iterations.

  a. Applies any of 12 combinations of spectral preprocessing methods using `DoPreprocessing()`.
![Preprocess](./man/figures/preprocess.png)

  b. Determines cross-validation scheme with `FormatCV()`. Choose from random, stratified random, or a plant breeding-specific scheme from [Jarqu&iacute;n et *al.*, 2017. *The Plant Genome*](https://doi.org/10.3835/plantgenome2016.12.0130).
![CV](./man/figures/cv_schemes.png)

  c. Trains spectral prediction models using `TrainSpectralModel()`.
   - Choose from partial least squares regression, random forest, and support vector machine algorithms
   - Uses 5-fold cross validation within the training set to tune model hyperparameters
   - Outputs model performance statistics (RMSE, R<sup>2</sup>, Bias, etc.) as assessed with test set

5. Save trained prediction models with `SaveModel()`.
  - Intended for a production environment
  - Can evaluate preprocessing methods using the input dataset
  - Selects best model using the metric provided (RMSE or  R<sup>2</sup>)
  - Returns trained model with option to save as .Rds object

6. Predict phenotypic values with new spectra and a saved model using `PredictFromSavedModel()`.


## Examples

The package comes with an example dataset from [Ikeogu et *al.* (2017) *PLoS ONE*](https://doi.org/10.1371/journal.pone.0188918) that can be used to try out package capabilities:

``` r
# Load and preview the example dataset (ikeogu.2017)
data(ikeogu.2017)
ikeogu.2017[1:10,1:10]

# Inspect and show the number of observations for each study within the `data.frame`
ikeogu.2017 %>% 
  group_by(study.name) %>% 
  nest() %>% 
  mutate(n.obs = map_dbl(data, ~nrow(.)))
  
# Plot reference value distributions
ikeogu.2017 %>% dplyr::select(-starts_with("X")) %>% 
  group_by(study.name) %>%
  gather(trait, value, c(DMC.oven:TCC), na.rm = T) %>%
  ggplot(aes(x= study.name, y = value, fill = study.name)) +
  facet_wrap(~ trait, scales='free_y', nrow=2) +
  geom_boxplot() +
  theme_bw()
```

### Example dataset reference distributions:
![Reference distributions](./man/figures/example_ref_dists_h.png)

A. Root dry matter content as measured by the oven method for the four studies included in the example dataset

B. Total carotenoid content as measured by HPLC

## Performance tests
```{r}
# Subset two of the studies from the dataset in preparation for model building
C16Mcal <- ikeogu.2017 %>% filter(study.name == "C16Mcal") %>% 
  rename(reference = DMC.oven) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% 
  na.omit()
C16Mval <- ikeogu.2017 %>% filter(study.name == "C16Mval") %>% 
  rename(reference = DMC.oven) %>%
  rename(unique.id = sample.id) %>%
  dplyr::select(unique.id, reference, starts_with("X")) %>% 
  na.omit()
  
# Then try out `TestModelPerformance()` to predict reference values from spectra
test_results <- TestModelPerformance(train.data = C16Mcal, 
                                     test.data = C16Mval,
                                     num.iterations = 10, 
                                     preprocessing = T, 
                                     summary = F,
                                     wavelengths = 350:2500)
```
### `waves` prediction model performance
![Pretreatment performance with example data](./man/figures/testplot_all_R2.png)

Distributions of R<sub>p</sub><sup>2</sup>, the squared Pearson’s correlation between predicted and observed for the test set, for partial least squares regression (PLSR) models of two root quality traits trained on samples from the C16Mcal dataset and tested on samples from the C16Mval dataset from [Ikeogu et *al.* (2017) *PLoS ONE*](https://doi.org/10.1371/journal.pone.0188918) with raw data or after pretreatment. 

*SNV: standard normal variate, SNV1D: standard normal variate and first derivative, SNV2D: standard normal variate and second derivative, D1: first derivative, D2: second derivative, SG: Savitzky-Golay with window size = 11, SNVSG: standard normal variate and Savitzky-Golay, SGD1: gap segment derivative with window size = 11, SG.D1W5: Savitzky-Golay with window size = 5 and first derivative, SG.D1W11: Savitzky-Golay with window size = 11 and first derivative, SG.D2W5: Savitzky-Golay with window size = 5 and second derivative, SG.D2W11: Savitzky-Golay with window size = 11 and second derivative. 





