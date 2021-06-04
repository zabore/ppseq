#' Output from a one-sample call to \code{calibrate_thresholds}
#'
#' This .rda file contains output from a one-sample call to 
#' \code{calibrate_thresholds()}. 
#' See the vignette titled "One-sample expansion cohort" for a description of
#' the input parameters used, or run 
#' \code{one_sample_cal_tbl$calibrate_thresholds_inputs}
#' to see a list of the original function inputs. For use in testing  
#' functions and in vignettes.
#'
#' @docType data
#'
#' @usage data(one_sample_cal_tbl)
#'
#' @format A list containing a 
#' 1) a tibble 'res_summary' containing the posterior probability threshold 
#' (pp_threshold), the predictive probability threshold (ppp_threshold), 
#' the mean sample size (mean_n0 and mean_n1 for two-sample case; 
#' mean_n1 for one-sample case), and the proportion of positive trials under 
#' the null and alternative response rates.
#' 2) 'call_list' containing the original function call
#' 3) 'calibrate_thresholds_inputs' a list containing the inputs to the 
#' original function call
#' 4) 'protocol_res' gives results from the protocol-specified design of
#' the atezolizumab case study, as detailed in the vignette titled 
#' "One-sample expansion cohort"
#'
#' @keywords datasets
"one_sample_cal_tbl"