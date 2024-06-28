#' signif_normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_signif_normal_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Module")
  )
}

#' signif_normal Server Functions
#'
#' @noRd
mod_signif_normal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_signif_normal_ui("signif_normal_1")

## To be copied in the server
# mod_signif_normal_server("signif_normal_1")
