#' sample_ratio
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @export create_srm_text


create_srm_text <- function(count_table, threshold){

  # Initialise local variables
  srm_p_val <- NULL
  options(scipen = 1000)

  srm_p_val <- chisq.test(count_table$sample_size,
                          p = count_table$expected_proportion)$p.value

  if(srm_p_val >= threshold){
    return(HTML(paste0('<font color = \"#1b680b\"><b>There is no sample ratio mismatch error</b></font> (p-value = ', signif(srm_p_val, 2), ')')))
  } else {
    return(HTML(paste0('<font color = \"#c30000\"><b>There is a sample ratio mismatch error</b></font> (p-value = ', signif(srm_p_val, 2),
                       ') <br>Note that this means that experiment results are more likely to be untrustworthy, and the cause for the mismatch should be investigated.')))
  }
}
