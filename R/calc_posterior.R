#' Calculate a single posterior probability
#'
#' @description This function is meant to be used in the context of a 
#' clinical trial with a binary endpoint. Interest is in the response rate. 
#' For the two-sample case, the total number of responses for the 
#' standard-of-care arm is y0 and the total number of responses for the 
#' experimental arm is y1. The function samples from the posterior beta 
#' distribution based on the data and the prior beta hyperparameters, and
#' returns the posterior probability that y1 is greater than (or less than) y0
#' given the data. The one-sample case is also available, in which a target p
#' must be specified and the function returns the posterior probability that
#' y1 is greater than (or less than) p given the data.
#'
#' @param y vector of length two containing total responses c(y0, y1) for 
#' two-sample case; integer of total responses for one-sample case
#' @param n vector of length two containing total sample size c(n0, n1) for 
#' two-sample case; integer of total sample size for one-sample case
#' @param direction "greater" (default) if interest is in p(y1 > y0) and "less" 
#' if interest is in p(y1 < y0). For one-sample case, "greater" if interest is 
#' in p(y1 > p) and "less" if interest is in p(y1 < p)
#' @param p The target value to compare to in the one-sample case
#' @param delta clinically meaningful difference between groups. 
#' Typically 0 (default).
#' @param prior hyperparameters of beta distribution, Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param seed set the seed for the random number generation
#' 
#' @return Returns the posterior probability of interest
#'
#' @examples
#'
#' # Two-sample case
#' calc_posterior(y = c(15, 23), n = c(100, 100))
#' 
#' # One-sample case
#' calc_posterior(y = 24, n = 100, direction = "greater", p = 0.2, delta = NULL)
#'
#' @export

calc_posterior <- function(y, n, direction = "greater", p = NULL, 
                           delta = 0, prior = c(0.5,0.5), S = 5000,
                           seed = 1) { 
  
  if(length(y) != length(n)) 
    stop("y and n must be the same length")
  
  if((is.null(p) & is.null(delta)) | (!is.null(p) & !is.null(delta)))
    stop("Exactly one of delta or p must be specified for the two-sample and 
         one-sample case, respectively")
  
  if(!direction %in% c("greater", "less")) 
    stop('direction must be either "greater" or "less"')
  
  if(length(y) == 1 & is.null(p))
    stop("p must be specified for the one-sample case")
  
  if(length(y) == 2) {
    
    rb0 <- stats::rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- stats::rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2]) 
    
    out <- ifelse(direction == "greater", 
                  mean(rb1 > rb0 + delta), 
                  mean(rb1 + delta < rb0)
                  )
    
  } else if(length(y) == 1) {
    
    rb1 <- stats::rbeta(S, prior[1] + y, prior[2] + n - y) 
    
    out <- ifelse(direction == "greater", mean(rb1 > p), mean(rb1 < p))
  }
  
  return(out)
}



# 
# # Function testing
# 
# 
# 
# # Two-sample case
# set.seed(20210216)
# 
# n <- c(100, 100)
# 
# y <- c(rbinom(size = n[1], n = 1, prob = 0.15),
#        rbinom(size = n[2], n = 1, prob = 0.25))
# 
# calc_posterior(y, n, direction = "greater", p = NULL, 
#                delta = 0, prior = c(0.5,0.5), S = 5000)
# 
# calc_posterior(y, n, direction = "less", p = NULL, 
#                delta = 0, prior = c(0.5,0.5), S = 5000)
# 
# calc_posterior(y, n, direction = "greater", p = NULL, 
#                delta = 0.1, prior = c(0.5,0.5), S = 5000)
# 
# calc_posterior(y, n = 100, direction = "greater", p = NULL, 
#                delta = 0, prior = c(0.5,0.5), S = 5000)
# 
# calc_posterior(y, n, direction = "greater", p = NULL, 
#                delta = NULL, prior = c(0.5,0.5), S = 5000)
# 
# calc_posterior(y, n, direction = "greater", p = 0.15, 
#                delta = 0, prior = c(0.5,0.5), S = 5000)
# 
# calc_posterior(y, n, direction = "equal", p = NULL, 
#                delta = 0, prior = c(0.5,0.5), S = 5000)
# 
# 
# # one-sample case
# set.seed(20210216)
# 
# n <- 100
# 
# y <- rbinom(size = n, n = 1, prob = 0.25)
# 
# calc_posterior(y, n, direction = "greater", p = 0.15, 
#                delta = NULL, prior = c(0.5,0.5), S = 5000)
