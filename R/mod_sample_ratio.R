#' sample_ratio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sample_ratio_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Module")
  )
}

#' sample_ratio Server Functions
#'
#' @noRd
mod_sample_ratio_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sample_ratio_ui("sample_ratio_1")

## To be copied in the server
# mod_sample_ratio_server("sample_ratio_1")
