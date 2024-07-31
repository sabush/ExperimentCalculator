#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import shinyWidgets
#' @import rhandsontable
#' @import dplyr
#' @import fresh
#' @import pwr
#' @import scales
#' @import plotly
#' @import ggplot2
#' @import stringr
#' @import forcats
#' @importFrom stats chisq.test pnorm power qnorm qt sd uniroot
#' @noRd

library(plotly)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(fresh)
library(rhandsontable)
library(ggplot2)
library(dplyr)
library(pwr)
library(scales)
library(stringr)
library(forcats)
library(stats)

options(scipen = 999)
colour_vec <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7')


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    navbarPage(
      title = "Power and Significance Calculator",
      header = tagList(
        fresh::use_theme(
          create_theme(
            theme = 'default',
            bs_vars_global(
              text_color = "#000"
            ),
            fresh::bs_vars_navbar(
              default_bg = "#0072B2",
              default_color = "#FFFFFF",
              default_link_color = "#FFFFFF",
              default_link_active_color = "#FFFFFF"
            ),
            fresh::bs_vars_color(
              brand_primary = '#0072B2'
            ),
            output_file = NULL
          )
        )
      ),
      tabPanel(
        title = "Power - Binary Outcome",
        mod_power_binary_ui("power_binary_1")
      ),

      tabPanel(
        title = "Power - Normal Outcome",
        mod_power_normal_ui("power_normal_1")
      ),

      tabPanel(
        title = "Significance - Binary Outcome",
        mod_signif_binary_ui("signif_binary_1")
      ),

      tabPanel(
        title = "Significance - Normal Outcome",
        mod_signif_normal_ui("signif_normal_1")
      ),

      tabPanel(
        title = "Sample ratio mismatch test",
        mod_sample_ratio_ui("sample_ratio_1")
      )#,

      # actionButton("browser", "browser"),
      # tags$script("$('#browser').hide();")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ExperimentCalculator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
