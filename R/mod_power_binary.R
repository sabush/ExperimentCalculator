#' power_binary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_power_binary_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Module")
  )
}

#' power_binary Server Functions
#'
#' @noRd
mod_power_binary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_power_binary_ui("power_binary_1")

## To be copied in the server
# mod_power_binary_server("power_binary_1")
