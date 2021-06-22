#' Calculate a single posterior probability
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. For the two-sample case, the total
#' number of events in the standard-of-care arm is y0 and the total number of
#' events in the experimental arm is y1. The function samples from the posterior
#' beta distribution based on the data and the prior beta hyperparameters, and
#' returns the posterior probability that p1 is greater than (or less than) p0
#' given the data. The one-sample case is also available, in which a target p0
#' must be specified and the function returns the posterior probability that
#' p is greater than (or less than) p0 given the data.
#'
#' @param y vector of length two containing total responses c(y0, y1) for
#' two-sample case; integer of total responses y for one-sample case
#' @param n vector of length two containing the sample size so far c(n0, n1)
#' for two-sample case; integer of sample size so far n for one-sample case
#' @param p0 The target value to compare to in the one-sample case. Set to NULL 
#' in two-sample case.
#' @param direction "greater" (default) if interest is in p(p1 > p0) and "less"
#' if interest is in p(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in p(p > p0) and "less" if interest is in p(p < p0).
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for two-sample case. NULL for one-sample case (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#'
#' @return Returns the posterior probability of interest
#'
#' @examples
#'
#' set.seed(123)
#'
#' # Two-sample case
#' calc_posterior(y = c(14, 23), n = c(100, 100), p0 = NULL, delta = 0)
#'
#' # One-sample case
#' calc_posterior(y = 27, n = 100, p0 = 0.2)
#' 
#' @importFrom stats rbeta
#' @export

calc_posterior <- function(y, n, p0, direction = "greater", delta = NULL, 
                           prior = c(0.5, 0.5), S = 5000) {
  if (length(y) != length(n))
    stop("y and n must be the same length")

  if ((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop("Exactly one of delta or p0 must be specified for the two-sample and
         one-sample case, respectively")

  if (!direction %in% c("greater", "less"))
    stop('direction must be either "greater" or "less"')

  if (length(y) == 1 & is.null(p0))
    stop("p0 must be specified for the one-sample case")

  if (length(y) == 2 & is.null(delta))
    stop("delta must be specified for the two-sample case")

  if (length(y) == 2) {
    rb0 <- rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2])

    out <- ifelse(direction == "greater",
      mean(rb1 > rb0 + delta),
      mean(rb1 + delta < rb0)
    )
  } else if (length(y) == 1) {
    rb1 <- rbeta(S, prior[1] + y, prior[2] + n - y)

    out <- ifelse(direction == "greater", mean(rb1 > p0), mean(rb1 < p0))
  }

  return(out)
}
