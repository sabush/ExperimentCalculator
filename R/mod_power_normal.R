#' power_normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_power_normal_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Module")
  )
}

#' power_normal Server Functions
#'
#' @noRd
mod_power_normal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_power_normal_ui("power_normal_1")

## To be copied in the server
# mod_power_normal_server("power_normal_1")
