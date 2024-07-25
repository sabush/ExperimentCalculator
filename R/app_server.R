#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_power_binary_server("power_binary_1")
  mod_power_normal_server("power_normal_1")
  mod_signif_binary_server("signif_binary_1")
  mod_signif_normal_server("signif_normal_1")
  mod_sample_ratio_server("sample_ratio_1")
}
