#' Calculate a decision rule table for interim monitoring of a pre-specified
#' design
#'
#' @description This function will take the posterior and predictive thresholds
#' for a pre-specified design with a given null response rate and fixed interim
#' looks and total sample size, and return the decision rules at each interim
#' analysis and the end of the trial. Intended for use after selecting an
#' optimal design using the functions \code{calibrate_thresholds} and
#' \code{optimize_design}.
#'
#' @param n integer of sample size so far for one-sample case
#' @param direction For one-sample case, "greater" if interest is in P(p > p0)
#' and "less" if interest is in P(p < p0).
#' @param p0 The target value to compare to in the one-sample case
#' (i.e. the null response rate)
#' @param delta clinically meaningful difference between groups.
#' Typically 0. NULL for one-sample case (default). Placeholder for when
#' two-sample case is added.
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param N tinteger of total planned sample size at end of trial N
#' for one-sample case
#' @param theta The target posterior probability. e.g. Efficacy decision if
#' P(p > p0) > theta for the one-sample case with greater direction.
#' @param ppp The target predictive probability. e.g. Stop the trial if the
#' predictive probability falls below this target.
#'
#' @return Returns a tibble with n at each look and r. Stop the trial at that
#' look if the number of observed responses is <=r. At the end of the trial,
#' the treatment is considered promising if the number of observed responses 
#' is >r.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#'
#' # One-sample case
#' calc_decision_rules(seq(5, 25, 5), p0 = 0.1, N = 25, theta = 0.86, ppp = 0.2)
#' }
#'
#' @importFrom tibble tibble
#' @export

calc_decision_rules <- function(n, direction = "greater", p0,
                                delta = NULL, prior = c(0.5, 0.5), S = 5000,
                                N, theta, ppp) {
  if ((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop(paste("Exactly one of delta or p0 must be specified for the", 
               "two-sample and one-sample case, respectively")

  if (!direction %in% c("greater", "less"))
    stop('direction must be either "greater" or "less"')

  # Set up the results table
  res <- tibble(
    n = n,
    r = rep(NA_integer_, length(n))
  )

  # initialize the ytest parameter (will have 1 added in loop so will really 
  # start at 0)


  # Loop over each value of n and for each, continue while the predictive 
  # probability for a given ytest is less than the ppp threshold.
  for (i in n) {
    ytest <- -1
    pred <- 0
    while (pred < ppp) {
      ytest <- ytest + 1
      pred <- calc_predictive(
        y = ytest,
        n = i,
        direction = direction,
        p0 = p0,
        delta = delta,
        prior = prior,
        S = S,
        N = N,
        theta = theta
      )
    }

    # Store the results
    res[i == n, 2] <- ytest <- ifelse(ytest == 0 & pred > ppp, NA, ytest - 1)
  }

  return(res)
}
