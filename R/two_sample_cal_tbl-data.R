#' Output from a two-sample call to \code{calibrate_thresholds}
#'
#' This .rda file contains output from a two-sample call to
#' \code{calibrate_thresholds()}.
#' See the vignette titled "Two-sample randomized trial" for a description of
#' the input parameters used, or run
#' \code{two_sample_cal_tbl$inputs}
#' to see a list of the original function inputs. For use in testing
#' functions and in vignettes.
#'
#' @docType data
#'
#' @usage data(two_sample_cal_tbl)
#'
#' @format A list containing a
#' 1) a tibble 'res_summary' containing the posterior probability threshold
#' (pp_threshold); the predictive probability threshold (ppp_threshold);
#' the mean sample size under the null (mean_n0_null and mean_n1_null) 
#' and alternative (mean_n0_alt and mean_n1_alt) response rates; 
#' the proportion of positive trials under the null (prop_pos_null) 
#' and alternative (prop_pos_alt) response rates; and the proportion of trials
#' stopped under the null (prop_stopped_null) and alternative (prop_stopped_alt)
#' response rates.
#' 2) 'call_list' containing the original function call
#' 3) 'inputs' a list containing the inputs to the
#' original function call
#'
#' @keywords datasets
"two_sample_cal_tbl"
