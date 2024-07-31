
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Experiment Calculator

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/sabush/ExperimentCalculator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sabush/ExperimentCalculator/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Experiment Calculator is to provide a simple calculator to
plan, analyse and diagnose simple experiments.

## Use shinyapps.io

This package allows you to run the app locally. If you don’t want to run
the dashboard locally, then the calculator is also available on
shinyapps.io here
(<https://stephen-bush.shinyapps.io/ExperimentCalculator/>)

## Installation

You can install the development version of ExperimentCalculator like so:

``` r
devtools::install_github('sabush/ExperimentCalculator')
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'ExperimentCalculator' from a github remote, the SHA1 (e5af4f95) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Load the Calculator

To load the power and significance calculator, load the library and run
the app

``` r
library(ExperimentCalculator)
#> Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
#> when loading 'ExperimentCalculator'
#ExperimentCalculator::run_app() # Uncomment to run the app
```

Further usage of the app can be found in the vignettes.

## Code of Conduct

Please note that the ExperimentCalculator project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
