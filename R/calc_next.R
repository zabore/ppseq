#' Calculate response probability for the next patient
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. For the two-sample case, the total
#' number of events in the standard-of-care arm is y0 and the total number of
#' events in the experimental arm is y1. The function samples from the posterior
#' beta distribution based on the data and the prior beta hyperparameters, and
#' returns the empiric mean and bootstrap confidence interval for the next
#' patient. The empiric mean represents the probability of the binary outcome
#' occurring in the next patient. 
#' The one-sample case is also available.
#'
#' @param y number of events observed so far. Vector of length two c(y0, y1) 
#' for the two-sample case; integer y for the one-sample case.
#' @param n sample size observed so far. Vector of length two c(n0, n1)
#' for the two-sample case; integer n for the one-sample case.
#' @param prior vector of length two containing hyperparameters of the prior 
#' beta distribution. c(0.5, 0.5) is default, for the Beta(0.5, 0.5) 
#' distribution.
#' @param S number of samples, default is 5000
#' @param interval a value between 0 and 1 indicating the width of the desired
#' interval, default is 0.95
#'
#' @return Returns a tibble with the group indicator (for the two-sample case 
#' only), the empiric mean, the bootstrap confidence interval, and the 
#' specified width of the confidence interval.
#'
#' @examples
#'
#' set.seed(123)
#' 
#' # One-sample case
#' calc_next(
#'   y = 27, 
#'   n = 100,
#'   S = 100
#'   )
#'
#' # Two-sample case
#' calc_next(
#'   y = c(14, 23), 
#'   n = c(100, 100),
#'   S = 100
#'   )
#' 
#' @importFrom stats rbeta quantile
#' @importFrom tibble tibble
#' @export
#' 

calc_next <- function(y, n, 
                      prior = c(0.5, 0.5), 
                      S = 5000,
                      interval = 0.95) {
  if (length(y) != length(n))
    stop("y and n must be the same length")
  
  if (interval <= 0 | interval >= 1)
    stop("interval must be a value between 0 and 1")
  
  if (length(y) == 2) {
    rb0 <- rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2])
    
    prob_next0 <- mean(
      map_dbl(rb0, rbinom, n = 1, size = 1)
    )
    prob_next1 <- mean(
      map_dbl(rb1, rbinom, n = 1, size = 1)
    )
    
    boots0  <- map_dbl(
      1:1000,
      ~mean(
        map_dbl(sample(rb0, length(rb0), replace = T), rbinom, n = 1, size = 1)
      )
    )
    boots1  <- map_dbl(
      1:1000,
      ~mean(
        map_dbl(sample(rb1, length(rb1), replace = T), rbinom, n = 1, size = 1)
      )
    )
    
    int0 <- quantile(boots0, c((1 - interval)/2, 1 - (1 - interval)/2))
    int1 <- quantile(boots1, c((1 - interval)/2, 1 - (1 - interval)/2))
    
    res <- tibble(
      Group = c(0, 1),
      Probability = c(prob_next0, prob_next1), 
      `Lower CI` = c(int0[[1]], int1[[1]]),
      `Upper CI` = c(int0[[2]], int1[[2]]),
      `CI` = rep(interval, 2)
    )
    
  } else if (length(y) == 1) {
    # posterior distribution of response rate
    rb1 <- rbeta(S, prior[1] + y, prior[2] + n - y)
    
    # Use the posterior probabilities to generate outcome in next patient
    # the average is the probability of response in the next patient
    prob_next <- mean(
      map_dbl(rb1, rbinom, n = 1, size = 1)
    )
    
    # take bootstrap samples of the posterior and repeat 1000 times
    boots <- 
      map_dbl(
        1:1000,
        ~mean(
          map_dbl(sample(rb1, length(rb1), replace = T), rbinom, n = 1, size = 1)
        )
      )
    
    # then take quantiles to get the confidence interval
    int <- quantile(boots, c((1 - interval)/2, 1 - (1 - interval)/2))
    
    # Put the results into a table
    res <- tibble(
      Probability = prob_next, 
      `Lower CI` = int[[1]],
      `Upper CI` = int[[2]],
      `CI` = interval
    )
  }
  return(res)
  
}
# set.seed(123)
# 
# # One-sample case
# calc_next(
#   y = 27,
#   n = 100
#   )
# 
# # Two-sample case
# calc_next(
#   y = c(14, 23),
#   n = c(100, 100)
#   )