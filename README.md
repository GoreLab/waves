# waves

<!-- badges: start -->
![language: R](https://img.shields.io/badge/language-R-blue.svg)
![CRAN/METACRAN](https://img.shields.io/cran/v/waves?label=CRAN)
[![R build status](https://github.com/GoreLab/waves/workflows/R-CMD-check/badge.svg)](https://github.com/GoreLab/waves/actions)
<!-- badges: end -->

Originally designed application in the context of resource-limited plant research and breeding programs, `waves` provides an open-source solution to spectral data processing and model development by bringing useful packages together into a streamlined pipeline. This package is wrapper for functions related to the analysis of point visible and near-infrared reflectance measurements. It includes visualization, filtering, aggregation, pretreatment, cross-validation set formation, model training, and prediction functions to enable open-source association of spectral and reference data. 

*Please note: function names were updated as of version 0.2.0. Old function names still work in this version but will be retired in upcoming package versions.*

## Cite
This package is documented in a peer-reviewed manuscript in the Plant Phenome Journal. Please cite the manuscript if you have found this package to be useful! 

> Hershberger, J, Morales, N, Simoes, CC, Ellerbrock, B, Bauchet, G, Mueller, LA, Gore MA. Making waves in Breedbase: An integrated spectral data storage and analysis pipeline for plant breeding programs. Plant Phenome J. 2021; 4:e20012. https://doi.org/10.1002/ppj2.20012


## Use

Follow the installation instructions below, and then go wild! Use `waves` to analyze your own data. Please report any bugs or feature requests by opening issues in this repository.

More detailed examples can be found in the package vignette, ["Getting started with waves"](articles/waves.html)


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

2. Visualize and filter spectra using `plot_spectra()` and `filter_spectra()`.

3. If you have more than one scan per unique identifier, aggregate the scans by mean or median with `aggregate_spectra()`.

4. Use `test_spectra()` to perform spectral pretreatment, cross-validation set formation, and model training functions over multiple iterations.
    - Applies any of 12 combinations of spectral pretreatment methods using `pretreat_spectra()`.
    - Determines cross-validation scheme with `format_cv()`. Choose from random, stratified random, or a plant breeding-specific scheme from [Jarqu&iacute;n et *al.*, 2017. *The Plant Genome*](https://doi.org/10.3835/plantgenome2016.12.0130).
    - Trains spectral prediction models using `train_spectra()`.
        - Choose from partial least squares regression, random forest, and support vector machine algorithms
        - Uses k-fold cross validation within the training set to tune model hyperparameters
        - Outputs model performance statistics (RMSE, R<sup>2</sup>, Bias, etc.) as assessed with test set

5. Save trained prediction models with `save_model()`.
    - Intended for a production environment
    - Can evaluate spectral pretreatment methods using the input dataset
    - Selects best model using the metric provided (RMSE or  R<sup>2</sup>)
    - Returns trained model with option to save as .Rds object

6. Predict phenotypic values with new spectra and a saved model using `predict_spectra()`.


## Examples

The package comes with an example dataset (`ikeogu.2017`) from [Ikeogu et *al.* (2017) *PLoS ONE*](https://doi.org/10.1371/journal.pone.0188918) that can be used to try out package capabilities. This dataset includes vis-NIR spectra from cassava roots as well as two reference phenotypes:

* Root dry matter content as measured by the oven method for the four studies included in the example dataset
* Total carotenoid content as measured by HPLC





