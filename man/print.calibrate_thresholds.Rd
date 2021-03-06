% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.calibrate_thresholds}
\alias{print.calibrate_thresholds}
\title{Print method for \code{calibrate_thresholds} objects}
\usage{
\method{print}{calibrate_thresholds}(x, type1_range = c(0.05, 0.1), minimum_power = 0.8, ...)
}
\arguments{
\item{x}{an object of class 'calibrate_thresholds', usually returned by the
\code{calibrate_thresholds} function}

\item{type1_range}{a vector specifying the minimum and maximum acceptable
type I error. Specify NULL to return the full range of resulting type I
error. Defaults to c(0.05, 0.1)}

\item{minimum_power}{a numeric between 0 and 1 specifying the minimum
acceptable power. Specify NULL to return the full range of resulting power.
Defaults to 0.8.}

\item{...}{ignored}
}
\value{
Returns a tibble
}
\description{
By default prints only the res_summary table from an object of
class 'calibrate_thresholds'. The table can be limited to a range of type 1
error and a minimum value of power using the arguments 'type1_range' and
'minimum_power' respectively.
}
\examples{
\dontrun{
set.seed(123)

cal_tbl <- calibrate_thresholds(
  p_null = 0.1, p_alt = 0.3,
  n = seq(5, 25, 5), N = 25, 
  pp_threshold = c(0.9, 0.95, 0.96, 0.98),
  ppp_threshold = seq(0.05, 0.2, 0.05),
  direction = "greater", delta = NULL,
  prior = c(0.5, 0.5), S = 5000, nsim = 1000
)

print(cal_tbl)
print(cal_tbl, type1_range = NULL, minimum_power = NULL)
}

}
