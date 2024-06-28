#' signif_binary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_signif_binary_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Module")
  )
}

#' signif_binary Server Functions
#'
#' @noRd
mod_signif_binary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_signif_binary_ui("signif_binary_1")

## To be copied in the server
# mod_signif_binary_server("signif_binary_1")
