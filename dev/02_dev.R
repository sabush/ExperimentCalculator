# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "power_binary", with_test = TRUE) # Name of the module
golem::add_module(name = "power_normal", with_test = TRUE) # Name of the module
golem::add_module(name = "signif_binary", with_test = TRUE) # Name of the module
golem::add_module(name = "signif_normal", with_test = TRUE) # Name of the module
golem::add_module(name = "sample_ratio", with_test = TRUE) # Name of the module

golem::browser_button()

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_utils("power_binary", with_test = TRUE)
golem::add_utils("power_normal", with_test = TRUE)
golem::add_utils("signif_binary", with_test = TRUE)
golem::add_utils("signif_normal", with_test = TRUE)
golem::add_utils("sample_ratio", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("Experiment_Calculator")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage("codecov")

# Create a summary readme for the testthat subdirectory
remotes::install_github('yonicd/covrpage')
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# GitLab CI
usethis::use_gitlab_ci()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()


# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
