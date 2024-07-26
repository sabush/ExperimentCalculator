
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Experiment Calculator

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/sabush/ExperimentCalculator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sabush/ExperimentCalculator/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Experiment Calculator is to provide a simple calculator to
plan, analyse and diagnose simple experiments.

## Installation

You can install the development version of ExperimentCalculator like so:

``` r
devtools::install_github('sabush/ExperimentCalculator')
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo sabush/ExperimentCalculator@HEAD
#> Installing package into 'C:/Users/steph/AppData/Local/R/cache/R/renv/library/ExperimentCalculator-50fb6c6f/R-4.2/x86_64-w64-mingw32'
#> (as 'lib' is unspecified)
```

## Load the Calculator

To load the power and significance calculator, load the library and run
the app

``` r
library(ExperimentCalculator)
#ExperimentCalculator::run_app() # Uncomment to run the app
```

Further usage of the app can be found in the vignettes.

## Code of Conduct

Please note that the ExperimentCalculator project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
