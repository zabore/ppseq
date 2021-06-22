#' Simulate a single trial with posterior probability monitoring
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. The goal is to simulate event counts
#' from the binomial distribution based on the number of patients
#' accrued at each interim look, and calculate the posterior predictive
#' probability of success (or futility) at the end of a trial,
#' given the data available at each interim analysis.
#'
#' @param p vector of length two containing the probability of event in
#' the standard of care and experimental arm c(p0, p1) for the two-sample case;
#' integer of event probability for one-sample case
#' @param n matrix containing the total number of patients accrued so far at
#' each interim look in the standard of care (column 1) and experimental
#' (column 2) arms for two-sample case; vector of sample size accrued so far
#' at each interim look for one-sample case. The last value should be equal to
#' the total sample size at the end of the trial.
#' If only a single look will be done
#' at the end of the trial, this can be a vector specifying the total sample
#' size c(N0, N1) for the two-sample case or an integer specifying the total
#' sample size N for the one-sample case.
#' @param direction "greater" (default) if interest is in P(p1 > p0) and "less"
#' if interest is in P(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in P(p > p0) and "less" if interest is in P(p < p0).
#' @param p0 The target value to compare to in the one-sample case
#' @param delta clinically meaningful difference between groups.
#' Typically 0 (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param theta The target posterior probability. e.g. Efficacy decision if
#' P(p1 > p0) > theta for the two-sample case with greater direction.
#' Default is 0.95. Can be a vector if interest is in selecting from among a
#' variety of thresholds.
#'
#' @return Returns a tibble with pp_threshold (i.e. theta, the target posterior
#' probability), number of responses, sample size,
#' posterior probability, and posterior predictive probability at each
#' look
#' 
#' @importFrom stats rbinom
#' @importFrom purrr pmap_dbl map2_dbl
#' @importFrom tibble tibble add_column
#' @importFrom furrr future_pmap_dbl furrr_options
#' @importFrom future nbrOfWorkers
#' @importFrom dplyr arrange select everything
#' 
#' @examples
#'
#' # One-sample case
#' set.seed(123)
#' sim_single_trial(
#'   p = 0.3, n = c(5, 10), direction = "greater",
#'   p0 = 0.1, delta = NULL, prior = c(0.5, 0.5), S = 50, N = 25, theta = 0.95
#' )
#'
#' # # Two-sample case (not run)
#' # set.seed(123)
#' #  sim_single_trial(
#' #    p = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)),
#' #    direction = "greater", p0 = NULL, delta = 0, prior = c(0.5, 0.5), 
#' #    S = 5000, N = c(50, 50), theta = 0.95
#' #  )
#' @export

sim_single_trial <- function(p, n, direction = "greater", p0 = NULL,
                             delta = 0, prior = c(0.5, 0.5), S = 5000,
                             N, theta = 0.95) {
  if ((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop("Exactly one of delta or p0 must be specified for the two-sample and
         one-sample case, respectively")

  if (!direction %in% c("greater", "less"))
    stop('direction must be either "greater" or "less"')

  if (length(p) == 1 & is.null(p0))
    stop("p0 must be specified for the one-sample case")

  if (length(p) == 2 & is.null(delta))
    stop("delta must be specified for the two-sample case")

  if (length(p) == 2) {
    if (length(n) == 2 & is.matrix(n) == FALSE) {
      n <- matrix(n, nrow = 1)
    }

    y0 <- rbinom(n = 1, size = n[1, 1], prob = p[1])
    y1 <- rbinom(n = 1, size = n[1, 2], prob = p[2])

    if (length(n) > 2) {
      for (i in seq_len(nrow(n))[-1]) {
        y0 <- c(y0, y0[length(y0)] + rbinom(
          n = 1,
          size = n[i, 1] - n[i - 1, 1],
          prob = p[1]
        ))
        y1 <- c(y1, y1[length(y1)] + rbinom(
          n = 1,
          size = n[i, 2] - n[i - 1, 2],
          prob = p[2]
        ))
      }
    }

    pp <- pmap_dbl(
      list(y0, y1, n[, 1], n[, 2]),
      ~ calc_posterior(
        y = c(..1, ..2),
        n = c(..3, ..4),
        p0 = p0, delta = delta, prior = prior,
        S = S
      )
    )

    crossargs <- tibble(
      y0 = rep(y0, length(theta)),
      y1 = rep(y1, length(theta)),
      n0 = rep(n[, 1], length(theta)),
      n1 = rep(n[, 2], length(theta)),
      pp_threshold = rep(theta, each = length(pp))
    )

    ppp <- pmap_dbl(
      crossargs,
      ~ calc_predictive(
        y = c(..1, ..2),
        n = c(..3, ..4),
        p0 = p0, delta = delta, prior = prior,
        S = S, N = N, theta = ..5
      )
    )

    res <- arrange(
      select(
        add_column(crossargs,
          pp = rep(pp, length(theta)),
          ppp = ppp
        ),
        pp_threshold, everything()
      ),
      pp_threshold
    )
  } else if (length(p) == 1) {
    y1 <- rbinom(n = 1, size = n[1], prob = p)

    if (length(n) > 1) {
      for (i in seq_along(n)[-1]) {
        y1 <- c(y1, y1[length(y1)] + rbinom(
          n = 1,
          size = n[i] - n[i - 1],
          prob = p
        ))
      }
    }

    pp <- map2_dbl(
      y1, n,
      ~ calc_posterior(
        y = .x,
        n = .y,
        p0 = p0, delta = delta, prior = prior,
        S = S
      )
    )

    crossargs <- tibble(
      y1 = rep(y1, length(theta)),
      n1 = rep(n, length(theta)),
      pp_threshold = rep(theta, each = length(pp))
    )

    ppp <- pmap_dbl(
      crossargs,
      ~ calc_predictive(
        y = ..1,
        n = ..2,
        p0 = p0, delta = delta, prior = prior,
        S = S, N = N, theta = ..3
      )
    )

    res <- arrange(
      select(
        add_column(crossargs, pp = rep(pp, length(theta)), ppp = ppp),
        pp_threshold, everything()
      ),
      pp_threshold
    )
  }
  return(res)
}
