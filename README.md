
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Experiment Calculator

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/sabush/ExperimentCalculator/branch/master/graph/badge.svg)](https://app.codecov.io/gh/sabush/ExperimentCalculator?branch=master)
[![R-CMD-check](https://github.com/sabush/ExperimentCalculator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sabush/ExperimentCalculator/actions/workflows/R-CMD-check.yaml)
[![CircleCI build
status](https://circleci.com/gh/sabush/ExperimentCalculator.svg?style=svg)](https://circleci.com/gh/sabush/ExperimentCalculator)
<!-- badges: end -->

The goal of Experiment Calculator is to provide a simple calculator to
plan, analyse and diagnose simple experiments.

## Installation

You can install the development version of ExperimentCalculator like so:

``` r
devtools::install_github('sabush/ExperimentCalculator')
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo sabush/ExperimentCalculator@HEAD
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> Rcpp    (1.0.12 -> 1.0.13) [CRAN]
#> tinytex (0.51   -> 0.52  ) [CRAN]
#> xfun    (0.45   -> 0.46  ) [CRAN]
#> yaml    (2.3.8  -> 2.3.9 ) [CRAN]
#> knitr   (1.47   -> 1.48  ) [CRAN]
#> Installing 5 packages: Rcpp, tinytex, xfun, yaml, knitr
#> Installing packages into 'C:/Users/steph/AppData/Local/R/cache/R/renv/library/ExperimentCalculator-50fb6c6f/R-4.2/x86_64-w64-mingw32'
#> (as 'lib' is unspecified)
#> package 'Rcpp' successfully unpacked and MD5 sums checked
#> package 'tinytex' successfully unpacked and MD5 sums checked
#> package 'xfun' successfully unpacked and MD5 sums checked
#> package 'yaml' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'yaml'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\steph\AppData\Local\R\cache\R\renv\library\ExperimentCalculator-50fb6c6f\R-4.2\x86_64-w64-mingw32\00LOCK\yaml\libs\x64\yaml.dll
#> to
#> C:\Users\steph\AppData\Local\R\cache\R\renv\library\ExperimentCalculator-50fb6c6f\R-4.2\x86_64-w64-mingw32\yaml\libs\x64\yaml.dll:
#> Permission denied
#> Warning: restored 'yaml'
#> package 'knitr' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\steph\AppData\Local\Temp\Rtmpsdi24N\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\steph\AppData\Local\Temp\Rtmpsdi24N\remotes6bdc70a07f8c\sabush-ExperimentCalculator-d7e8ebe/DESCRIPTION' ...  ✔  checking for file 'C:\Users\steph\AppData\Local\Temp\Rtmpsdi24N\remotes6bdc70a07f8c\sabush-ExperimentCalculator-d7e8ebe/DESCRIPTION'
#>       ─  preparing 'ExperimentCalculator':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'ExperimentCalculator_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/steph/AppData/Local/R/cache/R/renv/library/ExperimentCalculator-50fb6c6f/R-4.2/x86_64-w64-mingw32'
#> (as 'lib' is unspecified)
```

## Power Calculations

This is a basic example which shows you how to solve a common problem:

``` r
library(ExperimentCalculator)
## basic example code
```

## Code of Conduct

Please note that the ExperimentCalculator project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
