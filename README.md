
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
#> Downloading GitHub repo sabush/ExperimentCalculator@HEAD
#> Installing 3 packages: colorspace, bslib, yaml
#> Installing packages into 'C:/Users/steph/AppData/Local/R/cache/R/renv/library/ExperimentCalculator-50fb6c6f/R-4.2/x86_64-w64-mingw32'
#> (as 'lib' is unspecified)
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

## Dashboard View

![image](https://github.com/user-attachments/assets/627a656c-6007-4713-8734-a9dfdf0d57c0)

1. **Sample Size**: Calculates how many participants are needed to detect the effect.

2. **Baseline Success Rate**: Current conversion rate for the control group (e.g., 20%).

3. **Sample Split**: Percentage of traffic assigned to each group (e.g., 50/50).

4. **Effect Type — Absolute/Relative Effect**: Detects a specific increase (e.g., 5%).

5. **Minimum Detectable Effect**: Smallest effect size the test aims to detect (e.g., 5%).

6. **Significance Level**: Risk of false positives (e.g., 5%).

7. **Power Level**: Likelihood of detecting a true effect (e.g., 80%).

8. **Comparison Type**: Groups compared to the control.

9. **Correction**: Bonferroni adjustment to prevent false positives.

10. **Power Plot**: Visualizes the sample size needed (e.g., 2184 participants).


## Code of Conduct

Please note that the ExperimentCalculator project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
