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
    fluidPage(
      column(
        width = 4,

        p(HTML('<b>Summary data for each experiment group</b>')),
        p(HTML('To add additional groups, right click on the table and select <i>Insert Row Below</i>')),
        p(HTML('<i>label</i>: Group label for graphs, <br><i>sample_size</i>: Number of observations in group, <br><i>mean</i>: Average value for metric in group, <br><i>st_dev</i>: Standard deviation of observations in group')),
        br(),
        rhandsontable::rHandsontableOutput(ns('group_summary_hot')),
        br(),
        br(),
        radioButtons(ns("effect_type"), "Effect Type to be used",
                     choiceNames = c("Absolute Effect", "Relative Effect"),
                     choiceValues = c("abs", "rel")),
        shinyBS::bsTooltip(ns('effect_type'),
                  'An absolute effect is defined by the difference between the proportions in each group (new - old), where the relative effect is the percentage change in the outcome ((new - old)/old)'),

        shinyWidgets::numericInputIcon(ns("sig_lvl"), "Level of Significance", 5, 1, 20, 1,
                         icon = list(NULL, icon("percent"))),
        radioButtons(ns("comparisons"), "Which comparisons would you like to estimate",
                     choices = c("Compare to first" = "first", "Compare all pairs" = "all"),
                     selected = 'first'),
        radioButtons(ns("correction"), 'Multiple Comparisons Correction',
                     choices = c('Bonferroni' = 'bon', 'None' = 'none'),
                     selected = 'bon')
      ),

      column(
        width = 8,
        p(HTML('<b>Results</b>')),
        uiOutput(ns('text_string')),
        br(),
        br(),
        p(HTML('<b>Plots</b>')),
        br(),
        plotly::plotlyOutput(ns('diff_plot')),
        br(),
        br(),
        plotly::plotlyOutput(ns('group_plot'))
      )
    )
  )
}

#' signif_normal Server Functions
#'
#' @noRd
mod_signif_normal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    values <- reactiveValues(
      reset_summ_table = 0
    )

    # Create a reactive for the summary table
    sample_summ <- reactive({

      split_DF <- NULL
      if(!is.null(input$group_summary_hot)){ # If the table is populated, update
        summ_DF <- hot_to_r(input$group_summary_hot)
        values[['summ_DF']] <- summ_DF
      } else if (!is.null(isolate(values$summ_DF))) { # otherwise use the DF in values.
        summ_DF <- isolate(values$summ_DF)
      } else { # otherwise initialise the table
        summ_DF <- data.frame(label = c('Group 1', 'Group 2'),
                              sample_size = c(1000, 1000),
                              mean = c(4, 5),
                              st_dev = c(2, 2))
        values[['summ_DF']] <- summ_DF
        values$reset_summ_table <- 1
      }
      split_DF
    }) %>% debounce(1000)

    # Format the summary table
    output$group_summary_hot <- rhandsontable::renderRHandsontable({

      if (isolate(values$reset_summ_table) == 1) {
        summ_DF <- values$summ_DF
      } else {
        summ_DF <- sample_summ()
      }

      if(!is.null(summ_DF)){
        rhandsontable(summ_DF, width = 400) %>%
          hot_validate_numeric(cols = 'sample_size', min = 1) %>%
          hot_validate_numeric(cols = 'st_dev', min = 0) %>%
          hot_col('sample_size', format = "0") %>%
          hot_context_menu(allowColEdit = FALSE)
      }
    })


    # Return text output
    output$text_string <- renderUI({

      # Validations
      validate(
        need(!anyNA(values$summ_DF$label), 'Group labels should be defined for each group'),
        need(!anyNA(values$summ_DF$mean), 'A mean should be defined for each group'),
        need(min(values$summ_DF$sample_size) > 5, 'The sample size for each group must be greater than 5'),
        need(min(values$summ_DF$st_dev) > 0, 'Standard Deviations should be positive'),
        need(!is.na(input$sig_lvl), 'The significance level needs to be defined'),
        need(input$sig_lvl > 0, 'The significance level needs to be positive'),
        need(input$sig_lvl <= 20, 'The significance level needs to be less than 20%'),
        need(!is.na(input$correction), ''),
        need(!is.na(input$comparisons), '')
      )


      # Construct the text string with output

      summ_table <- values$summ_DF
      sig_lvl <- input$sig_lvl / 100

      # Apply p-value corrections
      if(input$correction == 'bon'){
        if(input$comparisons == 'first'){
          sig_corr = sig_lvl / (nrow(summ_table) - 1)
        } else {
          sig_corr = sig_lvl / (nrow(summ_table) * (nrow(summ_table) - 1) / 2)
        }
      } else {
        sig_corr = sig_lvl
      }

      # Return a different string based on input
      text_string <- construct_text_normal(
        summ_table = summ_table,
        eff_type = input$effect_type,
        comparisons = input$comparisons,
        sig = sig_corr)

      # Return the text string
      text_string
    })

    # Plot confidence intervals for differences

    output$diff_plot <- plotly::renderPlotly({
      summ_table <- values$summ_DF
      validate(
        need(!anyNA(values$summ_DF$label), ''),
        need(!anyNA(values$summ_DF$mean), ''),
        need(min(values$summ_DF$sample_size) > 5, ''),
        need(min(values$summ_DF$st_dev) > 0, ''),
        need(!is.na(input$sig_lvl), ''),
        need(input$sig_lvl > 0, ''),
        need(input$sig_lvl <= 20, ''),
        need(!is.na(input$correction), ''),
        need(!is.na(input$comparisons), '')
      )

      sig_lvl <- input$sig_lvl / 100

      # Apply p-value corrections
      if(input$correction == 'bon'){
        if(input$comparisons == 'first'){
          sig_corr = sig_lvl / (nrow(summ_table) - 1)
        } else {
          sig_corr = sig_lvl / (nrow(summ_table) * (nrow(summ_table) - 1) / 2)
        }
      } else {
        sig_corr = sig_lvl
      }

      construct_diff_plot_normal(
        summ_table = summ_table,
        eff_type = input$effect_type,
        comparisons = input$comparisons,
        sig = sig_corr,
        correction = input$correction)

    })


    # Plot confidence intervals for each group

    output$group_plot <- plotly::renderPlotly({
      summ_table <- values$summ_DF
      validate(
        need(!anyNA(values$summ_DF$label), ''),
        need(!anyNA(values$summ_DF$mean), ''),
        need(min(values$summ_DF$sample_size) > 5, ''),
        need(min(values$summ_DF$st_dev) > 0, ''),
        need(!is.na(input$sig_lvl), ''),
        need(input$sig_lvl > 0, ''),
        need(input$sig_lvl <= 20, ''),
        need(!is.na(input$correction), ''),
        need(!is.na(input$comparisons), '')
      )

      sig_lvl <- input$sig_lvl / 100

      # Apply p-value corrections
      if(input$correction == 'bon'){
        if(input$comparisons == 'first'){
          sig_corr = sig_lvl / (nrow(summ_table) - 1)
        } else {
          sig_corr = sig_lvl / (nrow(summ_table) * (nrow(summ_table) - 1) / 2)
        }
      } else {
        sig_corr = sig_lvl
      }

      construct_group_plot_normal(
        summ_table = summ_table,
        eff_type = input$effect_type,
        sig = sig_corr,
        correction = input$correction)

    })
  })
}
