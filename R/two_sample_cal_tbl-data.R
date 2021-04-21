#' Output from a two-sample call to \code{calibrate_threshold}
#'
#' This .rda file contains output from a two-sample call to 
#' \code{calibrate_threshold}. 
#' See the vignette titled "Two-sample randomized phase 2" for a description of
#' the input parameters used, or run 
#' \code{two_sample_cal_tbl$calibrate_thresholds_inputs}
#' to see a list of the original function inputs. For use in testing global 
#' functions and in vignettes.
#'
#' @docType data
#'
#' @usage data(two_sample_cal_tbl)
#'
#' @format A list containing a 
#' 1) a tibble 'res_summary' containing the posterior probability threshold 
#' (pp_threshold), the predictive probability threshold (ppp_threshold), 
#' the ceiling of the mean sample size (mean_n0 and mean_n1 for two-sample case; 
#' mean_n1 for one-sample case), and the proportion of positive trials.
#' 2) 'call_list' containing the original function call
#' 3) 'calibrate_thresholds_inputs' a list containing the inputs to the 
#' original function call
#' 4) 'simon_res' gives results from a Simon's two-stage minimax design with
#' unacceptable rate 0.1, acceptable rate 0.3, and theoretical type I error of
#' 0.05 and power of 0.8
#'
#' @keywords datasets
"two_sample_cal_tbl"