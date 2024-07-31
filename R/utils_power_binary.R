#' power_binary
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# @export power_diff_bin
# @export solve_power_bin_n
# @export es_diff_bin
# @export solve_prop_bin
# @export solve_power_bin_mde
# @export solve_power_helper_bin_ss
# @export solve_power_helper_bin_mde
# @export solve_power_all_pair_bin_ss
# @export solve_power_all_pair_bin_mde
# @export construct_text_pow_prop_ss
# @export construct_text_pow_prop_mde
# @export construct_power_curve_bin_ss
# @export construct_power_curve_bin_mde

#### Functions to perform calculations for binary power calculations ####

##### Calculate power in a useful format for optimiser #####

# Function to find the difference between observed and target power

power_diff_bin <- function(tot_ss, split, prop1, prop2, sig, pow_tgt){
  # Initialise local variables
  h_val <- NULL
  h_val <- pwr::ES.h(prop1, prop2) # Calculate the standardised effect size for two proportions
  return(pwr::pwr.2p2n.test(h_val, tot_ss * split, tot_ss * (1 - split), sig)$power - pow_tgt)
}

# Solve power problem for unknown sample size

solve_power_bin_n <- function(samp_prop, base_resp, mde, eff_type, sig, pow){
  # Initialise local variables
  prop1 <- NULL
  prop2 <- NULL
  range_min <- NULL
  range_max <- NULL
  root <- NULL

  # Calculate the second proportion
  if(eff_type == 'abs'){
    prop1 <- base_resp
    prop2 <- base_resp + mde
  } else if(eff_type == 'rel'){
    prop1 <- base_resp
    prop2 <- base_resp * (1 + mde)
  } else {
    stop('Invalid effect type')
  }

  # Set sensible minimum and maximum values for numerical optimisation
  range_min <- ceiling(max(8 / samp_prop, 8 / (1 - samp_prop)))
  range_max <- 1000000000

  # Return an error if the max will be exceeded
  if(pwr::pwr.2p2n.test(pwr::ES.h(prop1, prop2), range_max * samp_prop, range_max * (1 - samp_prop), sig)$power < 0.8){
    return("The required sample size exceeds 1,000,000,000")
  }

  # Return an error if the min not be met
  if(pwr::pwr.2p2n.test(pwr::ES.h(prop1, prop2), range_min * samp_prop, range_min * (1 - samp_prop), sig)$power > 0.8){
    return("The required sample size is less than 8")
  }

  # Apply solver
  root <- stats::uniroot(f = power_diff_bin,
                  interval = c(range_min, range_max),
                  split = samp_prop,
                  prop1 = prop1,
                  prop2 = prop2,
                  sig = sig,
                  pow_tgt = pow)

  return(ceiling(root$root))
}

# Solve power problem for unknown MDE

es_diff_bin <- function(p2, es, p1){
  return(es - pwr::ES.h(p2, p1))
}

solve_prop_bin <- function(es, p1){
  # Initialise local variables
  prop2 <- NULL

  prop2 <- uniroot(f = es_diff_bin,
                   interval = c(0, 1),
                   es = es,
                   p1 = p1)$root
  return(prop2)
}

solve_power_bin_mde <- function(samp_prop, base_resp, tot_ss, eff_type, sig, pow){

  # Initialise local variables
  eff_size <- NULL
  prop2 <- NULL

  # Solve the power calculation for an effect size
  eff_size <- pwr::pwr.2p2n.test(h = NULL, tot_ss * samp_prop, tot_ss * (1 - samp_prop), sig, power = pow)$h

  # Find the raw second proportion that corresponds to the effect size
  prop2 <- solve_prop_bin(eff_size, base_resp)

  # Convert to the desired effect type
  if(eff_type == 'abs'){
    return(prop2 - base_resp)
  } else if(eff_type == 'rel'){
    return((prop2 - base_resp) / base_resp)
  } else {
    stop('Invalid effect type')
  }
}

##### Solvers for a specific comparisons #####
solve_power_helper_bin_ss <- function(base_resp, samp_prop, mde, eff_type, sig, pow, comparison){

  # Initialise local variables
  split_string <- NULL
  group_1 <- NULL
  group_2 <- NULL
  samp_frac <- NULL
  samp_prop_pair <- NULL
  pair_ss <- NULL
  tot_ss <- NULL

  # Extract the requested comparison
  split_string <- str_split(comparison, " v ")
  group_1 <- as.numeric(split_string[[1]][1])
  group_2 <- as.numeric(split_string[[1]][2])

  samp_frac <- samp_prop[group_1] + samp_prop[group_2]
  samp_prop_pair <- samp_prop[group_1] / samp_frac

  # Calculate the sample size needed for pair
  pair_ss <- solve_power_bin_n(samp_prop = samp_prop_pair,
                               base_resp = base_resp,
                               mde = mde,
                               eff_type = eff_type,
                               sig = sig,
                               pow = pow)

  # Project back to total sample
  tot_ss <- ceiling(pair_ss / samp_frac)
  return(tot_ss)
}


solve_power_helper_bin_mde <- function(base_resp, samp_prop, tot_ss, eff_type, sig, pow, comparison){

  # Initialise local variables
  split_string <- NULL
  group_1 <- NULL
  group_2 <- NULL
  samp_frac <- NULL
  samp_prop_pair <- NULL
  pair_mde <- NULL

  # Extract the requested comparison
  split_string <- str_split(comparison, " v ")
  group_1 <- as.numeric(split_string[[1]][1])
  group_2 <- as.numeric(split_string[[1]][2])

  samp_frac <- samp_prop[group_1] + samp_prop[group_2]
  samp_prop_pair <- samp_prop[group_1] / samp_frac

  # Calculate the sample size needed for pair
  pair_mde <- solve_power_bin_mde(samp_prop = samp_prop_pair,
                                  base_resp = base_resp,
                                  tot_ss = tot_ss * samp_frac,
                                  eff_type = eff_type,
                                  sig = sig,
                                  pow = pow)

  return(pair_mde)
}

##### Solvers for all comparisons #####

solve_power_all_pair_bin_ss <- function(base_resp, samp_prop, mde, eff_type, sig, pow, pairs){

  # Initialise local variables
  ss_list <- NULL
  new_string <- NULL
  pair_ss <- NULL
  ss_list <- NULL

  ss_list <- c()
  if(pairs == 'first'){
    for(tmt2 in 2:(length(samp_prop))){
      new_string <- paste0(1, " v ", tmt2)
      pair_ss <- solve_power_helper_bin_ss(
        base_resp = base_resp,
        samp_prop = samp_prop,
        mde = mde,
        eff_type = eff_type,
        sig = sig,
        pow = pow,
        comparison = new_string)
      ss_list <- append(ss_list, pair_ss)
    }
  } else {
    for(tmt1 in (1:(length(samp_prop) - 1))){
      for(tmt2 in ((tmt1 + 1):(length(samp_prop)))){
        new_string <- paste0(tmt1, " v ", tmt2)
        pair_ss <- solve_power_helper_bin_ss(
          base_resp = base_resp,
          samp_prop = samp_prop,
          mde = mde,
          eff_type = eff_type,
          sig = sig,
          pow = pow,
          comparison = new_string)
        ss_list <- append(ss_list, pair_ss)
      }
    }
  }
  return(max(ss_list))
}

solve_power_all_pair_bin_mde <- function(base_resp, samp_prop, tot_ss, eff_type, sig, pow, pairs){

  # Initialise local variables
  mde_list <- NULL
  new_string <- NULL
  pair_mde <- NULL
  mde_list <- NULL

  mde_list <- c()
  if(pairs == 'first'){
    for(tmt2 in 2:(length(samp_prop))){
      new_string <- paste0(1, " v ", tmt2)
      pair_mde <- solve_power_helper_bin_mde(
        base_resp = base_resp,
        samp_prop = samp_prop,
        tot_ss = tot_ss,
        eff_type = eff_type,
        sig = sig,
        pow = pow,
        comparison = new_string)
      mde_list <- append(mde_list, pair_mde)
    }
  } else {
    for(tmt1 in 1:(length(samp_prop) - 1)){
      for(tmt2 in (tmt1 + 1):(length(samp_prop))){
        new_string <- paste0(tmt1, " v ", tmt2)
        pair_mde <- solve_power_helper_bin_mde(
          base_resp = base_resp,
          samp_prop = samp_prop,
          tot_ss = tot_ss,
          eff_type = eff_type,
          sig = sig,
          pow = pow,
          comparison = new_string)
        mde_list <- append(mde_list, pair_mde)
      }
    }
  }
  return(max(mde_list))
}

##### Construct text strings #####
# Functions to return a text string with results
construct_text_pow_prop_ss <-
  function(base_resp, samp_prop, mde, eff_type, sig, pow, pairs){

    # Initialise local variables
    req_ss <- NULL
    string_ret <- NULL

    req_ss <-
      solve_power_all_pair_bin_ss(base_resp, samp_prop, mde, eff_type, sig, pow, pairs)

    string_ret <-
      HTML(paste0('To measure a ', mde*100,
                  '% increase in a response rate of ', base_resp*100,
                  '%, with ', pow * 100, '% power and a ', signif(sig * 100, 2),
                  '% significance level, we would need a total sample size of ', req_ss,
                  ' distributed across the treatments (in the given proportions).'

      ))
    return(string_ret)
  }

construct_text_pow_prop_mde <-
  function(base_resp, samp_prop, tot_ss, eff_type, sig, pow, pairs){

    # Initialise local variables
    mde_calc <- NULL
    string_ret <- NULL

    mde_calc <-
      solve_power_all_pair_bin_mde(base_resp, samp_prop, tot_ss, eff_type, sig, pow, pairs)
    string_ret <-
      HTML(paste0('With a total sample size of ', tot_ss,
                  ', distributed across the treatments (in the given proportions), your will be able to estimate a ',
                  signif(mde_calc * 100, 3), '% increase in the response rate with ',
                  pow * 100, '% power and ', signif(sig * 100, 2), '% significance.'
      ))
    return(string_ret)
  }

##### Construct power curves #####
#  Functions for constructing power curves

construct_power_curve_bin_ss <-
  function(base_resp, samp_prop, mde, eff_type, sig, pow, comp){

    # Initialise local variables
    prop1 <- NULL
    prop2 <- NULL
    title_txt <- NULL
    split_string <- NULL
    group_1 <- NULL
    group_2 <- NULL
    samp_frac <- NULL
    samp_prop_pair <- NULL
    range_min <- NULL
    range_max <- NULL
    calc_val <- NULL
    power_table <- NULL
    x <- NULL
    samp_prop1 <- NULL
    samp_prop2 <- NULL

    if(eff_type == 'abs'){
      prop1 <- base_resp
      prop2 <- base_resp + mde
      title_txt <- paste0('Power Plot for a ', signif(mde * 100, 2), '% absolute increase in response rate')
    } else if(eff_type == 'rel'){
      prop1 <- base_resp
      prop2 <- base_resp * (1 + mde)
      title_txt <- paste0('Power Plot for a ', signif(mde * 100, 2), '% relative increase in response rate')
    }


    # Extract the requested comparison
    split_string <- stringr::str_split(comp, " v ")
    group_1 <- as.numeric(split_string[[1]][1])
    group_2 <- as.numeric(split_string[[1]][2])

    samp_frac <- samp_prop[group_1] + samp_prop[group_2]
    samp_prop_pair <- samp_prop[group_1] / samp_frac


    range_min <- solve_power_helper_bin_ss(base_resp, samp_prop, mde, eff_type, sig, 0.1, comp)
    range_max <- solve_power_helper_bin_ss(base_resp, samp_prop, mde, eff_type, sig, 0.99, comp)
    calc_val <- solve_power_helper_bin_ss(base_resp, samp_prop, mde, eff_type, sig, pow, comp)
    power_table <-
      tibble(x = round(seq(from = range_min, to = range_max, length.out = 200))) %>%
      rowwise() %>%
      mutate(
        prop1 = prop1,
        prop2 = prop2,
        samp_prop1 = samp_prop[group_1],
        samp_prop2 = samp_prop[group_2],
        sig = sig) %>%
      rowwise() %>%
      mutate(power = pwr::pwr.2p2n.test(pwr::ES.h(prop1, prop2), x * samp_prop1, x * samp_prop2, sig)$power)

    plot <- power_table %>%
      ggplot(aes(x = x, y = power)) + geom_line(colour = colour_vec[1]) +
      annotate("segment", x = calc_val, xend = calc_val, y = 0, yend = pow, colour = colour_vec[2]) +
      annotate("segment", x = 0, xend = calc_val, y = pow, yend = pow, colour = colour_vec[2]) +
      # geom_segment(aes(x = calc_val, xend = calc_val, y = 0, yend = pow), colour = colour_vec[2]) +
      # geom_segment(aes(x = 0, xend = calc_val, y = pow, yend = pow), colour = colour_vec[2]) +
      theme_classic(base_size = 14) + scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::comma) +
      labs(title = title_txt, x = 'Total Sample Size (All Groups)', y = 'Power')

    return(plotly::ggplotly(plot))

  }

construct_power_curve_bin_mde <-
  function(base_resp, samp_prop, tot_ss, eff_type, sig, pow, comp){

    # Initialise local variables
    title_txt <- NULL
    x_txt <- NULL
    split_string <- NULL
    group_1 <- NULL
    group_2 <- NULL
    samp_frac <- NULL
    samp_prop_pair <- NULL
    range_min <- NULL
    range_max <- NULL
    calc_val <- NULL
    power_table <- NULL
    plot <- NULL
    ss_prop1 <- NULL
    ss_prop2 <- NULL
    prop1 <- NULL
    prop2 <- NULL
    esh <- NULL
    x <- NULL

    # Extract the requested comparison
    split_string <- str_split(comp, " v ")
    group_1 <- as.numeric(split_string[[1]][1])
    group_2 <- as.numeric(split_string[[1]][2])

    range_min <- solve_power_helper_bin_mde(base_resp, samp_prop, tot_ss, eff_type, sig, 0.1, comp)
    range_max <- solve_power_helper_bin_mde(base_resp, samp_prop, tot_ss, eff_type, sig, 0.99, comp)
    calc_val <- solve_power_helper_bin_mde(base_resp, samp_prop, tot_ss, eff_type, sig, pow, comp)

    power_table <-
      tibble(x = seq(from = range_min, to = range_max, length.out = 200)) %>%
      rowwise() %>%
      mutate(
        prop1 = base_resp,
        prop2 = if_else(eff_type == 'abs', prop1 + x, prop1 * (1 + x)),
        ss_prop1 = tot_ss * samp_prop[group_1],
        ss_prop2 = tot_ss * samp_prop[group_2],
        sig = sig,
        esh = pwr::ES.h(prop1, prop2)) %>%
      rowwise() %>%
      mutate(power = pwr::pwr.2p2n.test(esh, ss_prop1, ss_prop2, sig)$power)

    if(eff_type == 'abs'){
      x_txt <- 'Effect Size (Absolute)'
    } else if(eff_type == 'rel'){
      x_txt <- 'Effect Size (Relative)'
    }

    title_txt <- paste0('Power Plot for a total sample size of ', tot_ss)

    plot <- power_table %>%
      ggplot(aes(x = x, y = power)) + geom_line(colour = colour_vec[1]) +
      annotate("segment", x = calc_val, xend = calc_val, y = 0, yend = pow, colour = colour_vec[2]) +
      annotate("segment", x = 0, xend = calc_val, y = pow, yend = pow, colour = colour_vec[2]) +
      theme_classic(base_size = 14) + scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      labs(title = "Power Plot", x = x_txt, y = 'Power')

    return(plotly::ggplotly(plot))
  }
