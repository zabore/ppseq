#' Calibrate the posterior probability threshold
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. For a vector of possible posterior
#' decision thresholds, the function simulates many trials and then calculates
#' the average number of times the posterior probability exceeds a given
#' threshold. In a null case, this will result in the type I error at a given
#' threshold. In an alternative case, this will result in the power at a given
#' threshold.
#'
#' @param p vector of length two containing the probability of event in
#' the standard of care and experimental arm c(p0, p1) for the two-sample case;
#' integer of event probability for one-sample case
#' @param N vector of length two containing the total sample size c(N0, N1)
#' for two-sample case; integer of sample size so far N for one-sample case
#' @param p0 The target value to compare to in the one-sample case. Set to 
#' NULL for the two-sample case.
#' @param direction "greater" (default) if interest is in p(p1 > p0) and "less"
#' if interest is in p(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in p(p > p0) and "less" if interest is in p(p < p0).
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for the two-sample case. NULL for the one-sample case (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples drawn from the posterior, and number of simulated
#' trials. Default is 5000
#' @param theta The target posterior probability thresholds to consider.
#' Integer or vector.
#'
#' @return Returns a tibble with the posterior probability threshold(s) and
#' associated proportion of positive trials.
#'
#' @examples
#'
#' set.seed(123)
#'
#' # One-sample case
#' calibrate_posterior_threshold(
#'   p = 0.1, N = 50, direction = "greater",
#'   p0 = 0.1, delta = NULL, prior = c(0.5, 0.5), S = 5000, theta = c(0.9, 0.95)
#' )
#'
#' \dontrun{
#' # Two-sample case
#' calibrate_posterior_threshold(
#' p = c(0.1, 0.1), N = c(50, 50),
#' direction = "greater", p0 = NULL, delta = 0, prior = c(0.5, 0.5),
#' S = 5000, theta = c(0.9, 0.95)
#' # )
#' }
#' @importFrom purrr map_dbl map2_dbl
#' @importFrom stats rbinom
#' @importFrom tibble tibble
#' @export

calibrate_posterior_threshold <- function(p, N, p0,
                                          direction = "greater", 
                                          delta = NULL, prior = c(0.5, 0.5),
                                          S = 5000, theta) {
  if (length(N) == 2) {
    y0 <- map_dbl(
      seq_len(S),
      ~ rbinom(n = 1, size = N[1], prob = p[1])
    )
    y1 <- map_dbl(
      seq_len(S),
      ~ rbinom(n = 1, size = N[2], prob = p[2])
    )

    posts <-
      map2_dbl(
        y0, y1,
        ~ calc_posterior(
          y = c(.x, .y), n = N,
          direction = direction, p0 = p0,
          delta = delta, prior = prior, S = S
        )
      )
  } else if (length(N) == 1) {
    y1 <- map_dbl(
      seq_len(S),
      ~ rbinom(n = 1, size = N, prob = p)
    )

    posts <-
      map_dbl(
        y1,
        ~ calc_posterior(
          y = .x, n = N,
          direction = direction, p0 = p0,
          delta = delta, prior = prior, S = S
        )
      )
  }

  proportion_positive <- map_dbl(theta, ~ mean(posts > .x))

  return(tibble(
    pp_threshold = theta,
    prop_pos = proportion_positive
  ))
}
