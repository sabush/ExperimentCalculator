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
    fluidPage(
      column(
        width = 4,

        p(HTML('<b>Expected proportions and actual counts in each experiment group</b>')),
        p(HTML('To add additional groups, right click on the table and select <i>Insert Row Below</i>')),
        p(HTML('<i>sample_size</i>: Number of observations in group, <br><i>expected_proportions</i>: Expected proportion of sample in each group')),
        br(),
        rHandsontableOutput(ns('srm_hot')),
        br(),
        br(),

        numericInputIcon(ns("srm_thresh"), "Threshold for sample ratio mismatch (p-value)", 0.1, 0.01, 1, 0.01,
                         icon = list(NULL, icon("percent"))),
      ),

      column(
        width = 8,
        uiOutput(ns('srm_check')),
      )
    )
  )
}

#' sample_ratio Server Functions
#'
#' @noRd
mod_sample_ratio_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    values <- reactiveValues(
      reset_srm_table = 0
    )

    # Create a reactive for the summary table
    srm_summ <- reactive({

      srm_DF <- NULL
      if(!is.null(input$srm_hot)){ # If the table is populated, update
        srm_DF <- hot_to_r(input$srm_hot)
        values[['srm_DF']] <- srm_DF
      } else if (!is.null(isolate(values$srm_DF))) { # otherwise use the DF in values.
        srm_DF <- isolate(values$srm_DF)
      } else { # otherwise initialise the table
        srm_DF <- data.frame(sample_size = c(1000, 1000),
                             expected_proportion = c(0.5, 0.5))
        values[['srm_DF']] <- srm_DF
        values$reset_srm_table <- 1
      }
      srm_DF
    }) %>% debounce(1000)

    # Format the summary table
    output$srm_hot <- renderRHandsontable({

      if (isolate(values$reset_srm_table) == 1) {
        srm_DF <- values$srm_DF
      } else {
        srm_DF <- srm_summ()
      }

      if(!is.null(srm_DF)){
        rhandsontable(srm_DF, width = 300) %>%
          hot_validate_numeric(col = 'sample_size', min = 1) %>%
          hot_validate_numeric(col = 'expected_proportion', min = 0, max = 1) %>%
          hot_col('sample_size', format = "0") %>%
          hot_col('expected_proportion', format = "0.0000") %>%
          hot_context_menu(allowColEdit = FALSE)
      }
    })


    # Return text output
    output$srm_check <- renderUI({

      # Validations
      validate(
        need(!anyNA(values$srm_DF$sample_size), 'Sample sizes should be defined for each group'),
        need(!anyNA(values$srm_DF$expected_proportion), 'Expected proportions should be defined for each group'),
        need(sum(values$srm_DF$expected_proportion) == 1, 'Expected proportions should add to equal 1'),
      )

      # Construct the text string with output
      srm_table <- values$srm_DF
      srm_thresh <- input$srm_thresh / 100
      create_srm_text(srm_table, srm_thresh)
    })
  })
}

## To be copied in the UI
# mod_sample_ratio_ui("sample_ratio_1")

## To be copied in the server
# mod_sample_ratio_server("sample_ratio_1")
