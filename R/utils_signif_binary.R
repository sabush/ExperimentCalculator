#' signif_binary
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# @export pair_abs_diff_summary_binary
# @export pair_rel_diff_summary_binary
# @export construct_text_row_binary
# @export construct_text_binary
# @export construct_diff_plot_binary
# @export construct_group_plot_binary


pair_abs_diff_summary_binary <- function(x1, x2, ss1, ss2, sig){

  # Initialise local variables
  prop1 <- NULL
  prop2 <- NULL
  se1 <- NULL
  se2 <- NULL
  point_est <- NULL
  sd_diff <- NULL
  t_stat <- NULL
  p_value <- NULL
  signif <- NULL
  LCI <- NULL
  UCI <- NULL

  prop1 <- x1 / ss1
  prop2 <- x2 / ss2
  se1 <- sqrt(prop1 * (1 - prop1) / ss1)
  se2 <- sqrt(prop2 * (1 - prop2) / ss2)
  point_est <- prop2 - prop1
  sd_diff <- sqrt(se1 **2 + se2 ** 2)
  t_stat <- point_est / sd_diff
  p_value <- 2 * pnorm(-abs(t_stat))
  signif <- dplyr::if_else(p_value <= sig, "significant", "not significant")
  LCI <- point_est - qnorm(1 - sig/2) * sd_diff
  UCI <- point_est + qnorm(1 - sig/2) * sd_diff
  return(list(signif = signif, LCI = LCI, UCI = UCI, point_est = point_est))
}

pair_rel_diff_summary_binary <- function(x1, x2, ss1, ss2, sig){

  # Initialise local variables
  prop1 <- NULL
  prop2 <- NULL
  se1 <- NULL
  se2 <- NULL
  point_est <- NULL
  sd_rat <- NULL
  t_stat <- NULL
  p_value <- NULL
  signif <- NULL
  LCI <- NULL
  UCI <- NULL

  prop1 <- x1 / ss1
  prop2 <- x2 / ss2

  # Calculate the point estimate of the relative effect
  point_est <- (prop2 - prop1) / prop1
  # Calculate the standard error for each group
  se1 <- sqrt(prop1 * (1 - prop1) / ss1)
  se2 <- sqrt(prop2 * (1 - prop2) / ss2)
  # Use the delta method to estimate the standard error for the relative effect
  sd_rat <- sqrt(1/(prop1 ** 2) * (se2 ** 2) + (prop2 ** 2) / (prop1 ** 4) * (se1 ** 2))

  t_stat <- point_est / sd_rat
  p_value <- 2 * pnorm(-abs(t_stat))
  signif <- dplyr::if_else(p_value <= sig, "significant", "not significant")
  LCI <- point_est - qnorm(1 - sig/2) * sd_rat
  UCI <- point_est + qnorm(1 - sig/2) * sd_rat
  return(list(signif = signif, LCI = LCI, UCI = UCI, point_est = point_est))
}

construct_text_row_binary <- function(name1, name2, x1, x2, ss1, ss2, sig, eff_type){

  # Initialise local variables
  sig_test <- NULL

  if(eff_type == 'abs'){
    sig_test <- pair_abs_diff_summary_binary(x1, x2, ss1, ss2, sig)
    return(paste0('The difference between ', name1, ' and ', name2, ' is ',
                  sig_test$signif, ', with a mean difference of ', signif(sig_test$point_est, 3) * 100,
                  '%, and confidence interval: (', signif(sig_test$LCI, 3) * 100, '%, ',
                  signif(sig_test$UCI, 3) * 100, '%)'))
  } else {
    sig_test <- pair_rel_diff_summary_binary(x1, x2, ss1, ss2, sig)
    return(paste0('The relative difference between ', name1, ' and ', name2, ' is ',
                  sig_test$signif, ', with a mean realtive difference of ', signif(sig_test$point_est * 100, 3),
                  '%, and confidence interval: (', signif(sig_test$LCI * 100, 3), '%, ',
                  signif(sig_test$UCI * 100, 3), '%)'))
  }
}


construct_text_binary <- function(summ_table, eff_type, comparisons, sig){

  # Initialise local variables
  num_groups <- NULL
  working_string <- NULL
  new_string <- NULL

  num_groups <- nrow(summ_table)
  working_string <- ''
  if(comparisons == 'first'){
    for(tmt2 in 2:num_groups){
      new_string <- construct_text_row_binary(name1 = summ_table[1, 'label'],
                                              name2 = summ_table[tmt2, 'label'],
                                              x1 = summ_table[1, 'num_success'],
                                              x2 = summ_table[tmt2, 'num_success'],
                                              ss1 = summ_table[1, 'sample_size'],
                                              ss2 = summ_table[tmt2, 'sample_size'],
                                              sig = sig,
                                              eff_type = eff_type)
      working_string <- paste0(working_string, new_string, '<br>')
    }
  } else {
    for(tmt1 in 1:(num_groups - 1)){
      for(tmt2 in (tmt1 + 1):num_groups){
        new_string <- construct_text_row_binary(name1 = summ_table[tmt1, 'label'],
                                                name2 = summ_table[tmt2, 'label'],
                                                x1 = summ_table[tmt1, 'num_success'],
                                                x2 = summ_table[tmt2, 'num_success'],
                                                ss1 = summ_table[tmt1, 'sample_size'],
                                                ss2 = summ_table[tmt2, 'sample_size'],
                                                sig = sig,
                                                eff_type = eff_type)
        working_string <- paste0(working_string, new_string, '<br>')
      }
    }
  }

  return(HTML(working_string))
}


construct_diff_plot_binary <- function(summ_table, eff_type, comparisons, sig, correction){

  # Initialise local variables
  num_groups <- NULL
  working_frame <- NULL
  title_text <- NULL
  base_plot <- NULL
  name1 <- NULL
  name2 <- NULL
  x1 <- NULL
  x2 <- NULL
  ss1 <- NULL
  ss2 <- NULL
  label <- NULL
  diff <- NULL
  LCI <- NULL
  UCI <- NULL

  num_groups <- nrow(summ_table)
  working_frame <- tibble::tibble(name1 = character(), name2 = character(),
                          x1 = numeric(), x2 = numeric(),
                          ss1 = numeric(), ss2 = numeric())
  if(comparisons == 'first'){
    for(tmt2 in 2:num_groups){
      working_frame <- rows_append(working_frame,
                                   tibble(name1 = summ_table[1, 'label'],
                                          name2 = summ_table[tmt2, 'label'],
                                          x1 = summ_table[1, 'num_success'],
                                          x2 = summ_table[tmt2, 'num_success'],
                                          ss1 = summ_table[1, 'sample_size'],
                                          ss2 = summ_table[tmt2, 'sample_size']))

    }
  } else {
    for(tmt1 in 1:(num_groups - 1)){
      for(tmt2 in (tmt1 + 1):num_groups){
        working_frame <- rows_append(working_frame,
                                     tibble(name1 = summ_table[tmt1, 'label'],
                                            name2 = summ_table[tmt2, 'label'],
                                            x1 = summ_table[tmt1, 'num_success'],
                                            x2 = summ_table[tmt2, 'num_success'],
                                            ss1 = summ_table[tmt1, 'sample_size'],
                                            ss2 = summ_table[tmt2, 'sample_size']))
      }
    }
  }

  # Create a title stating the confidence level and correction method
  if(correction == 'bon' & nrow(summ_table) > 2){
    title_text <- paste0('Differences in response rate between groups <br><sup>with ', signif((1 - sig) * 100, 3), '% (Bonferroni corrected) confidence intervals</sup>')
  } else if(nrow(summ_table) > 2) {
    title_text <- paste0('Differences in response rate between groups <br><sup>with ', signif((1 - sig) * 100, 3), '% (uncorrected) confidence intervals</sup>')
  } else {
    title_text <- paste0('Differences in response rate between groups <br><sup>with ', signif((1 - sig) * 100, 3), '% confidence intervals')
  }

  if(eff_type == 'abs'){

    working_frame <- working_frame %>%
      rowwise() %>%
      mutate(label = paste0(name2, ' - ', name1),
             diff = x2 / ss2 - x1 / ss1,
             LCI = pair_abs_diff_summary_binary(x1, x2, ss1, ss2, sig)$LCI,
             UCI = pair_abs_diff_summary_binary(x1, x2, ss1, ss2, sig)$UCI,
             label = factor(label))

    base_plot <- working_frame %>%
      ggplot(aes(x = label, y = diff, colour = fct_rev(label))) +
      geom_crossbar(aes(ymin = LCI, ymax = UCI), width = 0.1, fill = 'grey', colour = 'grey') +
      geom_point(size = 10) +
      geom_hline(aes(yintercept = 0), colour = 'red') +
      scale_x_discrete(labels = scales::label_wrap(15), guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(labels = scales::percent) +
      scale_colour_manual(values = colour_vec) +
      theme_classic(base_size = 14) +
      theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0),
            legend.position = "none") +
      coord_flip() +
      labs(title = title_text,
           x = "", y = 'Absolute Difference in Response Rate')
  } else {
    working_frame <- working_frame %>%
      rowwise() %>%
      mutate(label = paste0(name2, ' - ', name1),
             diff = pair_rel_diff_summary_binary(x1, x2, ss1, ss2, sig)$point_est,
             LCI = pair_rel_diff_summary_binary(x1, x2, ss1, ss2, sig)$LCI,
             UCI = pair_rel_diff_summary_binary(x1, x2, ss1, ss2, sig)$UCI,
             label = factor(label))

    base_plot <- working_frame %>%
      ggplot(aes(x = fct_rev(label), y = diff, colour = fct_rev(label))) +
      geom_crossbar(aes(ymin = LCI, ymax = UCI), width = 0.1, fill = 'grey', colour = 'grey') +
      geom_point(size = 10) +
      geom_hline(aes(yintercept = 0), colour = 'red') +
      scale_x_discrete(labels = scales::label_wrap(15), guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(labels = scales::percent) +
      scale_colour_manual(values = colour_vec) +
      theme_classic(base_size = 14) +
      theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0),
            legend.position = "none") +
      coord_flip() +
      labs(title = title_text,
           x = "", y = 'Relative Difference in Response Rate')
  }
  return(plotly::ggplotly(base_plot))

}

construct_group_plot_binary <- function(summ_table, eff_type, sig, correction){

  # Initialise local variables
  summ_table <- NULL
  title_text <- NULL
  base_plot <- NULL
  prop <- NULL
  label <- NULL
  LCI <- NULL
  UCI <- NULL
  num_success <- NULL
  sample_size <- NULL

  summ_table <- summ_table %>%
    rowwise() %>%
    mutate(prop = num_success/sample_size,
           LCI = prop - qnorm(1 - sig/2) * sqrt(prop * (1 - prop) / sample_size),
           UCI = prop + qnorm(1 - sig/2) * sqrt(prop * (1 - prop) / sample_size),
           label = factor(label))

  # Create a title stating the confidence level and correction method
  if(correction == 'bon' & nrow(summ_table) > 2){
    title_text <- paste0('Group Response Rates <br><sup>with ', signif((1 - sig) * 100, 3), '% (Bonferroni corrected) confidence intervals</sup>')
  } else if(nrow(summ_table) > 2) {
    title_text <- paste0('Group Response Rates <br><sup>with ', signif((1 - sig) * 100, 3), '% (uncorrected) confidence intervals</sup>')
  } else {
    title_text <- paste0('Group Response Rates <br><sup>with ', signif((1 - sig) * 100, 3), '% confidence intervals')
  }

  base_plot <- summ_table %>%
    ggplot(aes(x = fct_rev(label), y = prop, colour = fct_rev(label))) +
    geom_crossbar(aes(ymin = LCI, ymax = UCI), width = 0.1, fill = 'grey', colour = 'grey') +
    geom_point(size = 10) +
    scale_x_discrete(labels = scales::label_wrap(15), guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(labels = scales::percent) +
    scale_colour_manual(values = colour_vec) +
    theme_classic(base_size = 14) +
    theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0),
          legend.position = "none") +
    coord_flip() +
    labs(title = title_text,
         x = "", y = 'Response rate')

  return(plotly::ggplotly(base_plot))
}
