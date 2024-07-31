#' signif_normal
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @export pair_abs_diff_summary_normal
#' @export pair_rel_diff_summary_normal
#' @export construct_text_row_normal
#' @export construct_text_normal
#' @export construct_diff_plot_normal
#' @export construct_group_plot_normal


pair_abs_diff_summary_normal <- function(mean1, mean2, sd1, sd2, ss1, ss2, sig){

  # Initialise local variables
  point_est <- NULL
  sd_diff <- NULL
  t_stat <- NULL
  p_value <- NULL
  signif <- NULL
  LCI <- NULL
  UCI <- NULL

  point_est <- mean2 - mean1
  sd_diff <- sqrt(((ss1 - 1) * (sd1 ** 2) + (ss2 - 1) * (sd2 ** 2)) / (ss1 + ss2 - 2)) * sqrt(1 / ss1 + 1 / ss2)
  t_stat <- point_est / sd_diff
  p_value <- 2 * pnorm(-abs(t_stat))
  signif <- dplyr::if_else(p_value <= sig, "significant", "not significant")
  LCI <- point_est - qt(1 - sig/2, ss1 + ss2 - 2) * sd_diff
  UCI <- point_est + qt(1 - sig/2, ss1 + ss2 - 2) * sd_diff
  return(list(signif = signif, LCI = LCI, UCI = UCI, point_est = point_est))
}

pair_rel_diff_summary_normal <- function(mean1, mean2, sd1, sd2, ss1, ss2, sig){

  # Initialise local variables
  point_est <- NULL
  se1 <- NULL
  se2 <- NULL
  sd_rat <- NULL
  t_stat <- NULL
  p_value <- NULL
  signif <- NULL
  LCI <- NULL
  UCI <- NULL


  # Calculate the point estimate of the relative effect
  point_est <- (mean2 - mean1) / mean1
  # Calculate the standard error for each group
  se1 <- sd1 /sqrt(ss1)
  se2 <- sd2 /sqrt(ss2)
  # Use the delta method to estimate the standard error for the relative effect
  sd_rat <- sqrt(1/(mean1 ** 2) * (se2 ** 2) + (mean2 ** 2) / (mean1 ** 4) * (se1 ** 2))

  t_stat <- point_est / sd_rat
  p_value <- 2 * pnorm(-abs(t_stat))
  signif <- dplyr::if_else(p_value <= sig, "significant", "not significant")
  LCI <- point_est - qt(1 - sig/2, ss1 + ss2 - 2) * sd_rat
  UCI <- point_est + qt(1 - sig/2, ss1 + ss2 - 2) * sd_rat
  return(list(signif = signif, LCI = LCI, UCI = UCI, point_est = point_est))
}

construct_text_row_normal <- function(name1, name2, mean1, mean2, sd1, sd2, ss1, ss2, sig, eff_type){

  # Initialise local variables
  sig_test <- NULL

  if(eff_type == 'abs'){
    sig_test <- pair_abs_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)
    return(paste0('The difference between ', name1, ' and ', name2, ' is ',
                  sig_test$signif, ', with a mean difference of ', signif(sig_test$point_est, 3),
                  ', and confidence interval: (', signif(sig_test$LCI, 3), ', ',
                  signif(sig_test$UCI, 3), ')'))
  } else {
    sig_test <- pair_rel_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)
    return(paste0('The relative difference between ', name1, ' and ', name2, ' is ',
                  sig_test$signif, ', with a mean realtive difference of ', signif(sig_test$point_est * 100, 3),
                  '%, and confidence interval: (', signif(sig_test$LCI * 100, 3), '%, ',
                  signif(sig_test$UCI * 100, 3), '%)'))
  }
}


construct_text_normal <- function(summ_table, eff_type, comparisons, sig){

  # Initialise local variables
  num_groups <- NULL
  working_string <- NULL
  new_string <- NULL

  num_groups <- nrow(summ_table)
  working_string <- ''
  if(comparisons == 'first'){
    for(tmt2 in 2:num_groups){
      new_string <- construct_text_row_normal(name1 = summ_table[1, 'label'],
                                              name2 = summ_table[tmt2, 'label'],
                                              mean1 = summ_table[1, 'mean'],
                                              mean2 = summ_table[tmt2, 'mean'],
                                              sd1 = summ_table[1, 'st_dev'],
                                              sd2 = summ_table[tmt2, 'st_dev'],
                                              ss1 = summ_table[1, 'sample_size'],
                                              ss2 = summ_table[tmt2, 'sample_size'],
                                              sig = sig,
                                              eff_type = eff_type)
      working_string <- paste0(working_string, new_string, '<br>')
    }
  } else {
    for(tmt1 in 1:(num_groups - 1)){
      for(tmt2 in (tmt1 + 1):num_groups){
        new_string <- construct_text_row_normal(name1 = summ_table[tmt1, 'label'],
                                                name2 = summ_table[tmt2, 'label'],
                                                mean1 = summ_table[tmt1, 'mean'],
                                                mean2 = summ_table[tmt2, 'mean'],
                                                sd1 = summ_table[tmt1, 'st_dev'],
                                                sd2 = summ_table[tmt2, 'st_dev'],
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


construct_diff_plot_normal <- function(summ_table, eff_type, comparisons, sig, correction){

  # Initialise local variables
  num_groups <- NULL
  working_frame <- NULL
  title_text <- NULL
  base_plot <- NULL

  num_groups <- nrow(summ_table)
  working_frame <- tibble(name1 = character(), name2 = character(),
                          mean1 = numeric(), mean2 = numeric(),
                          sd1 = numeric(), sd2 = numeric(),
                          ss1 = numeric(), ss2 = numeric())
  if(comparisons == 'first'){
    for(tmt2 in 2:num_groups){
      working_frame <- rows_append(working_frame,
                                   tibble(name1 = summ_table[1, 'label'],
                                          name2 = summ_table[tmt2, 'label'],
                                          mean1 = summ_table[1, 'mean'],
                                          mean2 = summ_table[tmt2, 'mean'],
                                          sd1 = summ_table[1, 'st_dev'],
                                          sd2 = summ_table[tmt2, 'st_dev'],
                                          ss1 = summ_table[1, 'sample_size'],
                                          ss2 = summ_table[tmt2, 'sample_size']))

    }
  } else {
    for(tmt1 in 1:(num_groups - 1)){
      for(tmt2 in (tmt1 + 1):num_groups){
        working_frame <- rows_append(working_frame,
                                     tibble(name1 = summ_table[tmt1, 'label'],
                                            name2 = summ_table[tmt2, 'label'],
                                            mean1 = summ_table[tmt1, 'mean'],
                                            mean2 = summ_table[tmt2, 'mean'],
                                            sd1 = summ_table[tmt1, 'st_dev'],
                                            sd2 = summ_table[tmt2, 'st_dev'],
                                            ss1 = summ_table[tmt1, 'sample_size'],
                                            ss2 = summ_table[tmt2, 'sample_size']))
      }
    }
  }

  # Create a title stating the confidence level and correction method
  if(correction == 'bon' & nrow(summ_table) > 2){
    title_text <- paste0('Differences in outcome between groups <br><sup>with ', signif((1 - sig) * 100, 3), '% (Bonferroni corrected) confidence intervals</sup>')
  } else if(nrow(summ_table) > 2) {
    title_text <- paste0('Differences in outcome between groups <br><sup>with ', signif((1 - sig) * 100, 3), '% (uncorrected) confidence intervals</sup>')
  } else {
    title_text <- paste0('Differences in outcome between groups <br><sup>with ', signif((1 - sig) * 100, 3), '% confidence intervals')
  }

  if(eff_type == 'abs'){

    working_frame <- working_frame %>%
      rowwise() %>%
      mutate(label = paste0(name2, ' - ', name1),
             mean = mean2 - mean1,
             LCI = pair_abs_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)$LCI,
             UCI = pair_abs_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)$UCI,
             label = factor(label))

    base_plot <- working_frame %>%
      ggplot2::ggplot(aes(x = label, y = mean, colour = forcats::fct_rev(label))) +
      geom_crossbar(aes(ymin = LCI, ymax = UCI), width = 0.1, fill = 'grey', colour = 'grey') +
      geom_point(size = 10) +
      geom_hline(aes(yintercept = 0), colour = 'red') +
      scale_x_discrete(labels = scales::label_wrap(15), guide = guide_axis(n.dodge = 2)) +
      scale_colour_manual(values = colour_vec) +
      theme_classic(base_size = 14) +
      theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0),
            legend.position = "none") +
      coord_flip() +
      labs(title = title_text,
           x = "", y = 'Absolute Difference in Outcome')
  } else {
    working_frame <- working_frame %>%
      rowwise() %>%
      mutate(label = paste0(name2, ' - ', name1),
             mean = pair_rel_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)$point_est,
             LCI = pair_rel_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)$LCI,
             UCI = pair_rel_diff_summary_normal(mean1, mean2, sd1, sd2, ss1, ss2, sig)$UCI,
             label = factor(label))

    base_plot <- working_frame %>%
      ggplot(aes(x = fct_rev(label), y = mean, colour = fct_rev(label))) +
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
           x = "", y = 'Relative Difference in Outcome')
  }
  return(ggplotly(base_plot))

}

construct_group_plot_normal <- function(summ_table, eff_type, sig, correction){

  # Initialise local variables
  summ_table <- NULL
  title_text <- NULL
  base_plot <- NULL

  summ_table <- summ_table %>%
    rowwise() %>%
    mutate(LCI = mean - qt(1 - sig/2, sample_size - 1) * st_dev /sqrt(sample_size),
           UCI = mean + qt(1 - sig/2, sample_size - 1) * st_dev /sqrt(sample_size),
           label = factor(label))

  # Create a title stating the confidence level and correction method
  if(correction == 'bon' & nrow(summ_table) > 2){
    title_text <- paste0('Group Means for Outcome <br><sup>with ', signif((1 - sig) * 100, 3), '% (Bonferroni corrected) confidence intervals</sup>')
  } else if(nrow(summ_table) > 2) {
    title_text <- paste0('Group Means for Outcome <br><sup>with ', signif((1 - sig) * 100, 3), '% (uncorrected) confidence intervals</sup>')
  } else {
    title_text <- paste0('Group Means for Outcome <br><sup>with ', signif((1 - sig) * 100, 3), '% confidence intervals')
  }

  base_plot <- summ_table %>%
    ggplot(aes(x = fct_rev(label), y = mean, colour = fct_rev(label))) +
    geom_crossbar(aes(ymin = LCI, ymax = UCI), width = 0.1, fill = 'grey', colour = 'grey') +
    geom_point(size = 10) +
    scale_x_discrete(labels = scales::label_wrap(15), guide = guide_axis(n.dodge = 2)) +
    scale_colour_manual(values = colour_vec) +
    theme_classic(base_size = 14) +
    theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0),
          legend.position = "none") +
    coord_flip() +
    labs(title = title_text,
         x = "", y = 'Mean Outcome')

  return(ggplotly(base_plot))
}
