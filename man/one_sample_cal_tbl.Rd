% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_sample_cal_tbl-data.R
\docType{data}
\name{one_sample_cal_tbl}
\alias{one_sample_cal_tbl}
\title{Output from a one-sample call to \code{calibrate_thresholds}}
\format{
A list containing a
\enumerate{
\item a tibble 'res_summary' containing the posterior probability threshold
(pp_threshold), the predictive probability threshold (ppp_threshold),
the mean sample size (mean_n0 and mean_n1 for two-sample case;
mean_n1 for one-sample case), and the proportion of positive trials under
the null and alternative response rates.
\item 'call_list' containing the original function call
\item 'calibrate_thresholds_inputs' a list containing the inputs to the
original function call
\item 'protocol_res' gives results from the protocol-specified design of
the atezolizumab case study, as detailed in the vignette titled
"One-sample expansion cohort"
}
}
\usage{
data(one_sample_cal_tbl)
}
\description{
This .rda file contains output from a one-sample call to
\code{calibrate_thresholds()}.
See the vignette titled "One-sample expansion cohort" for a description of
the input parameters used, or run
\code{one_sample_cal_tbl$calibrate_thresholds_inputs}
to see a list of the original function inputs. For use in testing
functions and in vignettes.
}
\keyword{datasets}
