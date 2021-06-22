#' Calculate a single posterior predictive value
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. The goal is to calculate the posterior
#' predictive probability of success at the end of a trial, given the data
#' available at an interim analysis. For the two-arm case the number of events
#' observed at interim analysis, the sample size at interim analysis,
#' and the total planned sample size are denoted y0, n0, and N0 in
#' the standard-of-care arm and y1, n1, and N1 in the experimental arm.
#'
#' @param y vector of length two containing number of events observed so far
#' c(y0, y1) for two-sample case; integer of number of events y observed so far
#' for one-sample case
#' @param n vector of length two containing the sample size so far c(n0, n1)
#' for two-sample case; integer of sample size so far for one-sample case
#' @param p0 The target value to compare to in the one-sample case. Set to NULL
#' for the two-sample case.
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param direction "greater" (default) if interest is in P(p1 > p0) and "less"
#' if interest is in P(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in P(p > p0) and "less" if interest is in P(p < p0).
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for the two-sample case. NULL for one-sample case (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param theta The target posterior probability. e.g. Efficacy decision if
#' P(p1 > p0) > theta for the two-sample case with greater direction.
#' Default is 0.95.
#'
#' @return Returns the posterior predictive probability of interest
#'
#' @examples
#'
#' set.seed(123)
#'
#' # One-sample case (not run)
#' # calc_predictive(y = 14, n = 50, p0 = 0.2, N = 100)
#'
#' # # Two-sample case (not run)
#' # calc_predictive(y = c(7, 12), n = c(50, 50), p0 = NULL, N = c(100, 100),
#' # delta = 0)
#' @importFrom stats rbeta rbinom
#' @importFrom purrr map_dbl map2_dbl
#' @export

calc_predictive <- function(y, n, p0, N, 
                            direction = "greater", delta = NULL, 
                            prior = c(0.5, 0.5), S = 5000,
                            theta = 0.95) {
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

  if (length(y) != length(N))
    stop("y and N must be the same length")

  if (length(y) == 2) {
    rb0 <- rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2])

    if (n[1] < N[1]) {
      Y0 <- y[1] + map_dbl(rb0, rbinom, n = 1, size = N[1] - n[1])
    } else {
      Y0 <- rep(y[1], S)
      N[1] <- n[1]
    }

    if (n[2] < N[2]) {
      Y1 <- y[2] + map_dbl(rb1, rbinom, n = 1, size = N[2] - n[2])
    } else {
      Y1 <- rep(y[2], S)
      N[2] <- n[2]
    }

    post <- map2_dbl(Y0, Y1, ~ calc_posterior(
      y = c(.x, .y), n = N,  direction = direction, p0 = p0, delta = delta,
      prior = prior, S = S
    ))
  } else if (length(y) == 1) {
    rb1 <- rbeta(S, prior[1] + y, prior[2] + n - y)

    if (n < N) {
      Y <- y + map_dbl(rb1, rbinom, n = 1, size = N - n)
    } else {
      Y <- rep(y, S)
      N <- n
    }

    post <- map_dbl(Y, calc_posterior,
      n = N,
      direction = direction, p0 = p0,
      delta = delta, prior = prior, S = S
    )
  }

  return(mean(post > theta))
}
