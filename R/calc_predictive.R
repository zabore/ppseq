#' Calculate a single posterior predictive value
#'
#' @description This function is meant to be used in the context of a 
#' clinical trial with a binary endpoint. Interest is in the response rate. 
#' The goal is to calculate the posterior predictive probability of success
#' at the end of a trial, given the data available at an interim analysis. 
#' For the two-arm case the number of responses observed at interim analysis,
#' the sample size at interim analysis, and the total planned sample size are 
#' denoted y0, n0, and N0 in the standard-of-care arm and y1, n1, and N1 in the
#' experimental arm. 
#'
#' @param y vector of length two containing number of responses observed so far
#' c(y0, y1) for two-sample case; integer of number of responses observed so far
#' for one-sample case
#' @param n vector of length two containing the sample size sor far c(n0, n1) 
#' for two-sample case; integer of sample size so far for one-sample case
#' @param direction "greater" (default) if interest is in p(y1 > y0) and "less" 
#' if interest is in p(y1 < y0). For one-sample case, "greater" if interest is 
#' in p(y1 > p) and "less" if interest is in p(y1 < p)
#' @param p The target value to compare to in the one-sample case
#' @param delta clinically meaningful difference between groups. 
#' Typically 0 (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param seed set the seed for the random number generation (default NULL)
#' @param N the total planned sample size at the end of the trial, c(N0, N!)
#' for two-sample case; integer of total planned sample size at end of trial
#' for one-sample case
#' @param theta The target posterior probability, default is 0.95
#' 
#' @return Returns the posterior predictive probability of interest
#'
#' @examples
#'
#' # Two-sample case
#' calc_predictive(y = c(7, 12), n = c(50, 50), seed = 1, N = c(100, 100))
#' 
#' # One-sample case
#' calc_predictive(y = 14, n = 50, p = 0.2, delta = NULL, seed = 1, N = 100)
#'
#' @export

calc_predictive <- function(y, n, direction = "greater", p = NULL, 
                            delta = 0, prior = c(0.5,0.5), S = 5000, 
                            seed = NULL, 
                            N, theta = 0.95) {
  
  if(length(y) != length(n)) 
    stop("y and n must be the same length")
  
    if((is.null(p) & is.null(delta)) | (!is.null(p) & !is.null(delta)))
    stop("Exactly one of delta or p must be specified for the two-sample and 
         one-sample case, respectively")
  
  if(!direction %in% c("greater", "less")) 
    stop('direction must be either "greater" or "less"')
  
  if(length(y) == 1 & is.null(p))
    stop("p must be specified for the one-sample case")
  
  if(length(y) == 2 & is.null(delta))
    stop("delta must be specified for the two-sample case")
  
  if(length(y) != length(N))
    stop("y and N must be the same length")
  
  set.seed(seed)
  
  if(length(y) == 2) {
    
    rb0 <- stats::rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- stats::rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2]) 
    
    if(n[1] < N[1]) {
      Y0 <- y[1] + sapply(rb0, stats::rbinom, n = 1, size = N[1] - n[1])
    } else {
      Y0 <- rep(y[1], S)
      N[1] <- n[1] 
    }
    
    if(n[2] < N[2]) {
      Y1 <- y[2] + sapply(rb1, stats::rbinom, n = 1, size = N[2] - n[2])
    } else {
      Y1 <- rep(y[2], S)
      N[2] <- n[2]
    }
    
    Y <- cbind(Y0,Y1)
    
    
    post <- apply(Y, MARGIN = 1, FUN = calc_posterior, n = N, 
                  direction = direction, p = p, 
                  delta = delta, prior = prior, S = S, 
                  seed = seed)

  } else if(length(y) == 1) {
    
    rb1 <- stats::rbeta(S, prior[1] + y, prior[2] + n - y)
    
    if(n < N) {
      Y <- y + sapply(rb1, stats::rbinom, n = 1, size = N - n)
    } else {
      Y <- rep(y, S)
      N <- n
    }
    
    post <- sapply(Y, FUN = calc_posterior, n = N, 
                  direction = direction, p = p, 
                  delta = delta, prior = prior, S = S, 
                  seed = seed)
    
  }
  
  return(mean(post > theta))
}





# #### Posterior Design #####
# 
# # prob y1 < y0 | Data #
# posterior_y0.gt.y1 <- function(y, n0, n1, delta=0, prior=c(0.5,0.5)){ ## y0 standard     y1 experimental
#   SAMP <- 5000
#   out <- sum( rbeta(SAMP, prior[1] + y[1], prior[2] + n0 - y[1]) > ( rbeta(SAMP, prior[1] + y[2], prior[2] + n1 - y[2]) + delta ) ) / SAMP
#   return(out)
# }
# 
# # pred y1 < y0 | Data #
# # d = y0,y1,n0,n1
# pred_y0.gt.y1 <- function(d, N0, N1, theta=0.95, delta=0, prior=c(0.5,0.5)){
#   
#   samp.Trial <- function(p, n){ return( rbinom( 1, n, p) ) }
#   
#   SAMP <- 5000
#   p0 <- rbeta(SAMP, prior[1] + d[1], prior[2] + d[3] - d[1])
#   p1 <- rbeta(SAMP, prior[1] + d[2], prior[2] + d[4] - d[2])
#   
#   if( d[3] < N0 ){ Y0 <- d[1] + sapply(p0, FUN=samp.Trial, n=N0-d[3])  
#   } else{ Y0 <- rep(d[1], SAMP); N0 <- d[3] }
#   if( d[4] < N1 ){ Y1 <- d[2] + sapply(p1, FUN=samp.Trial, n=N1-d[4])
#   } else{ Y1 <- rep(d[2], SAMP); N1 <- d[4] }
#   
#   y <- cbind(Y0,Y1)
#   
#   post <- apply( y, MARGIN=1, FUN=posterior_y0.gt.y1, n0=N0, n1=N1, delta=0, prior=c(0.5,0.5) )
#   return( sum( as.numeric( post > theta  ) )/SAMP )
# }
# 
# 
# # Make sure the above two functions give the same result
# set.seed(1)
# pred_y0.gt.y1(d = c(11, 7, 50, 50), N0 = 100, N1 = 100, theta=0.95, delta=0, prior=c(0.5,0.5))
# 
# 
# set.seed(1)
# calc_predictive(y = c(11, 7), n = c(50, 50), direction = "less", p = NULL, 
#                             delta = 0, prior = c(0.5,0.5), S = 5000, 
#                             seed = NULL, 
#                             N = c(100, 100), theta = 0.95)
# # Yes! They do, but only when direction = 'less' for calc_predictive since that is the default in Brian's function
# 
# 
# 
# 
# # Function testing
# 
# 
# 
# # Two-sample case
# set.seed(20210216)
# 
# n <- c(50, 50)
# 
# y <- c(rbinom(size = n[1], n = 1, prob = 0.15),
#        rbinom(size = n[2], n = 1, prob = 0.25))
# 
# d <- c(y, n)
# 
# N0 <- 100
# N1 <- 100
# 
# theta <- 0.95
# delta <- 0
# prior <- c(0.5, 0.5)
# 
# # pred_y0.gt.y1(d, N0, N1, theta=0.95, delta=0, prior=c(0.5,0.5))
# 
# samp.Trial <- function(p, n){ return( rbinom( 1, n, p) ) }
# 
# # First sample from the posterior beta distribution based on the data so far and the prior hyperparameters
# SAMP <- 5000
# p0 <- rbeta(SAMP, prior[1] + d[1], prior[2] + d[3] - d[1])
# p1 <- rbeta(SAMP, prior[1] + d[2], prior[2] + d[4] - d[2])
# 
# # Then if this is an interim analysis (i.e. haven't reached total planned sample size yet) we sample counts of the number of successes we may see in the remaining part of the trial based on the posterior probabilities generated above and the remaining planned sample size, and we add these to what we have seen so far to get a total number of possible successes for the trial. However if we have reached or exceeded the planned sample size then we fix the number of successes at what we have seen so far and the sample size at the true sample size we have observed.
# if( d[3] < N0 ){ Y0 <- d[1] + sapply(p0, FUN=samp.Trial, n=N0-d[3])  
# } else{ Y0 <- rep(d[1], SAMP); N0 <- d[3] }
# if( d[4] < N1 ){ Y1 <- d[2] + sapply(p1, FUN=samp.Trial, n=N1-d[4])
# } else{ Y1 <- rep(d[2], SAMP); N1 <- d[4] }
# 
# y <- cbind(Y0,Y1)
# 
# # Then for each of those total counts, we calculate the posterior probability that y1 > y0. So this is a posterior probability of success at the end of the trial
# post <- apply( y, MARGIN=1, FUN=posterior_y0.gt.y1, n0=N0, n1=N1, delta=0, prior=c(0.5,0.5) )
# 
# # And it returns the average number of trials where this probability exceeds a pre-specified value (with default of 0.95)
# sum( as.numeric( post > theta  ) )/SAMP 
# 
