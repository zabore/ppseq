#' Output from a one-sample call to \code{calc_decision_rules}
#'
#' This .rda file contains output from a one-sample call to
#' \code{calc_decision_rules()}.
#' See the vignette titled "One-sample expansion cohort" for a description of
#' the input parameters used.
#'
#' @docType data
#'
#' @usage data(one_sample_decision_tbl)
#'
#' @format A tibble containing n, the number of patients enrolled at each
#' futility monitoring point, and r, the number of responses at which we would
#' stop the trial at a given look if the number of observed responses is <=r,
#' or at the end of the trial the treatment is considered promising if the
#' number of observed responses is >r.
#'
#' @keywords datasets
"one_sample_decision_tbl"
