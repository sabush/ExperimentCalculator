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
    fluidPage(
      column(
        width = 4,
        radioButtons(ns("calc_type"), "What do you want to calculate",
                     choiceNames = c("Minimum Sample Size", "Minimum Detectible Effect"),
                     choiceValues = c("mss", "mde")),

        # Specify sample size if MDE being calculated
        conditionalPanel(condition = "input.calc_type == 'mde'",
                         numericInput(ns('sample_size'), 'Total Available Sample Size', 10000),
                         ns = ns,
                         bsTooltip(ns('sample_size'),
                                   'How many observations are available across all groups in the experiment?'),
        ),

        numericInputIcon(ns('prop_base'), 'Success rate in the baseline group', 20,
                         icon = list(NULL, icon("percent"))),
        bsTooltip(ns('prop_base'),
                  'What is the outcome rate in the baseline group of the experiment? This can usually be determined using existing data.'),

        p(HTML('<b>Sample split across experiment groups</b>')),
        p(HTML('To add additional groups, right click on the table and select <i>Insert Row Below</i>')),
        br(),
        rHandsontableOutput(ns('sample_split_hot')),
        br(),
        br(),
        radioButtons(ns("effect_type"), "Effect Type to be used",
                     choiceNames = c("Absolute Effect", "Relative Effect"),
                     choiceValues = c("abs", "rel")),
        bsTooltip(ns('effect_type'),
                  'An absolute effect is defined by the difference between the proportions in each group (new - old), where the relative effect is the percentage change in the outcome ((new - old)/old)'),

        # Specify MDE if sample size being calculated - absolute effect
        conditionalPanel(condition = "input.calc_type == 'mss' & input.effect_type == 'abs'",
                         numericInputIcon(ns('min_effect_abs'), 'Minimum Detectible Effect', 5,
                                          icon = list(NULL, icon("percent"))),
                         ns = ns,
                         bsTooltip(ns('min_effect_abs'),
                                   'What is the smallest absolute effect size that would be considered meaningful for the experiments? Note that the smaller this effect, the larger the sample size will be.'),
        ),

        # Specify MDE if sample size being calculated - absolute effect
        conditionalPanel(condition = "input.calc_type == 'mss' & input.effect_type == 'rel'",
                         numericInputIcon(ns('min_effect_rel'), 'Minimum Detectible Effect', 5,
                                          icon = list(NULL, icon("percent"))),
                         ns = ns,
                         bsTooltip(ns('min_effect_rel'),
                                   'What is the smallest relative effect size that would be considered meaningful for the experiments? Note that the smaller this effect, the larger the sample size will be.'),
        ),

        numericInputIcon(ns("sig_lvl"), "Level of Significance", 5, 1, 20, 1,
                         icon = list(NULL, icon("percent"))),
        numericInputIcon(ns("pwr_lvl"), "Level of Power", 80, 60, 95, 5,
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
        uiOutput(ns('text_string')),
        br(),
        selectizeInput(ns('treatment_pair'), 'Comparison for Power Curve', choices = NULL),
        plotlyOutput(ns('power_curve'))
      )
    )
  )
}

#' power_binary Server Functions
#'
#' @noRd
mod_power_binary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    values <- reactiveValues(
      reset_split_table = 0
    )

    # Create a reactive for the sample proportions table
    sample_split <- reactive({

      split_DF <- NULL
      if(!is.null(input$sample_split_hot)){ # If the table is populated, update
        split_DF <- hot_to_r(input$sample_split_hot)
        values[['split_DF']] <- split_DF
      } else if (!is.null(isolate(values$split_DF))) { # otherwise use the DF in values.
        split_DF <- isolate(values$split_DF)
      } else { # otherwise initialise the table
        split_DF <- data.frame(traffic_propotions = c(0.5, 0.5))
        values[['split_DF']] <- split_DF
        values$reset_split_table <- 1
      }
      split_DF
    }) %>% debounce(1000)

    # Format the sample proportions table
    output$sample_split_hot <- renderRHandsontable({

      if (isolate(values$reset_split_table) == 1) {
        split_DF <- values$split_DF
      } else {
        split_DF <- sample_split()
      }

      if(!is.null(split_DF)){
        rhandsontable(split_DF, width = 300) %>%
          hot_validate_numeric(col = 'traffic_propotions',
                               min = 0, max = 1) %>%
          hot_col('traffic_propotions', format = "0.0%") %>%
          hot_context_menu(allowColEdit = FALSE)
      }
    })

    # Create a set of pairs of treatments for power curves
    observe({
      prop_table <- values$split_DF
      prop_list <- prop_table$traffic_propotions

      tmt_pair_list <- c()
      if(input$comparisons == 'first'){
        for(tmt2 in 2:(length(prop_list))){
          new_string <- paste0(1, " v ", tmt2)
          tmt_pair_list <- append(tmt_pair_list, new_string)
        }
      } else {
        for(tmt1 in 1:(length(prop_list) - 1)){
          for(tmt2 in (tmt1 + 1):(length(prop_list))){
            new_string <- paste0(tmt1, " v ", tmt2)
            tmt_pair_list <- append(tmt_pair_list, new_string)
          }
        }
      }

      updateSelectizeInput(session,
                           'treatment_pair',
                           'Comparison for Power Curve',
                           choices = tmt_pair_list)
    })


    # Return text output
    output$text_string <- renderUI({

      # Validations
      validate(
        need(!anyNA(values$values$split_DF), 'Sample proportions should be defined for each group.'),
        need(sum(values$summ_DF$sample_size) == 1, 'The sample proportions should add to 100%.'),
        need(min(values$summ_DF$sample_size) > 0, 'The sample proportions for each group must be greater than 0.'),
        need(!is.na(input$sig_lvl), 'The significance level needs to be defined'),
        need(input$sig_lvl > 0, 'The significance level needs to be positive'),
        need(input$sig_lvl <= 20, 'The significance level needs to be less than 20%'),
        need(input$pwr_lvl < 100, 'The power level needs to be less than 100%'),
        need(input$pwr_lvl >= 60, 'The power level needs to be at least 60%, and ideally greater than 80%'),
        need(!is.na(input$correction), ''),
        need(!is.na(input$comparisons), '')
      )

      # Construct the text string with output

      # Convert proportion table into proportions
      prop_table <- values$split_DF
      prop_list <- prop_table$traffic_propotions

      prop_base <- input$prop_base / 100
      pwr_lvl <- input$pwr_lvl / 100
      min_effect_abs <- input$min_effect_abs / 100
      min_effect_rel <- input$min_effect_rel / 100
      sig_lvl <- input$sig_lvl / 100

      # Apply p-value corrections
      if(input$correction == 'bon'){
        if(input$comparisons == 'first'){
          sig_corr = sig_lvl / (length(prop_list) - 1)
        } else {
          sig_corr = sig_lvl / (length(prop_list) * (length(prop_list) - 1) / 2)
        }
      } else {
        sig_corr = sig_lvl
      }

      # Return a different string based on input
      if(input$calc_type == 'mss'){
        if(input$effect_type == 'abs'){
          text_string <- construct_text_pow_prop_ss(base_resp = prop_base,
                                                    samp_prop = prop_list,
                                                    mde = min_effect_abs,
                                                    eff_type = input$effect_type,
                                                    sig = sig_corr,
                                                    pow = pwr_lvl,
                                                    pairs = input$comparisons)
        } else {
          text_string <- construct_text_pow_prop_ss(base_resp = prop_base,
                                                    samp_prop = prop_list,
                                                    mde = min_effect_rel,
                                                    eff_type = input$effect_type,
                                                    sig = sig_corr,
                                                    pow = pwr_lvl,
                                                    pairs = input$comparisons)
        }
      } else{
        text_string <- construct_text_pow_prop_mde(base_resp = prop_base,
                                                   samp_prop = prop_list,
                                                   tot_ss = input$sample_size,
                                                   eff_type = input$effect_type,
                                                   sig = sig_corr,
                                                   pow = pwr_lvl,
                                                   pairs = input$comparisons)
      }

      # Return the text string
      text_string
    })

    # Construct a Power Curve
    output$power_curve <- renderPlotly({

      # Validations
      validate(
        need(!anyNA(values$values$split_DF), ''),
        need(sum(values$summ_DF$sample_size) == 1, ''),
        need(min(values$summ_DF$sample_size) > 0, ''),
        need(!is.na(input$sig_lvl), ''),
        need(input$sig_lvl > 0, ''),
        need(input$sig_lvl <= 20, ''),
        need(input$pwr_lvl < 100, ''),
        need(input$pwr_lvl >= 60, ''),
        need(!is.na(input$correction), ''),
        need(!is.na(input$comparisons), ''),
        need(!is.na(input$treatment_pair), ''),
        need(input$treatment_pair != '', '')
      )

      # Convert proportion table into proportions
      prop_table <- values$split_DF
      prop_list <- prop_table$traffic_propotions

      prop_base <- input$prop_base / 100
      pwr_lvl <- input$pwr_lvl / 100
      min_effect_abs <- input$min_effect_abs / 100
      min_effect_rel <- input$min_effect_rel / 100
      sig_lvl <- input$sig_lvl / 100

      # Apply p-value corrections
      if(input$correction == 'bon'){
        if(input$comparisons == 'first'){
          sig_corr = sig_lvl / (length(prop_list) - 1)
        } else {
          sig_corr = sig_lvl / (length(prop_list) * (length(prop_list) - 1) / 2)
        }
      } else {
        sig_corr = sig_lvl
      }

      if(input$calc_type == 'mss'){
        if(input$effect_type == 'abs'){
          construct_power_curve_bin_ss(base_resp = prop_base,
                                       samp_prop = prop_list,
                                       mde = min_effect_abs,
                                       eff_type = input$effect_type,
                                       sig = sig_corr,
                                       pow = pwr_lvl,
                                       comp = input$treatment_pair)
        } else {
          construct_power_curve_bin_ss(base_resp = prop_base,
                                       samp_prop = prop_list,
                                       mde = min_effect_rel,
                                       eff_type = input$effect_type,
                                       sig = sig_corr,
                                       pow = pwr_lvl,
                                       comp = input$treatment_pair)
        }
      } else {
        construct_power_curve_bin_mde(base_resp = prop_base,
                                      samp_prop = prop_list,
                                      tot_ss = input$sample_size,
                                      eff_type = input$effect_type,
                                      sig = sig_corr,
                                      pow = pwr_lvl,
                                      comp = input$treatment_pair)
      }

    })

  }
  )
}
