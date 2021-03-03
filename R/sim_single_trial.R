#' Simulate a single trial with posterior probability monitoring
#'
#' @description This function is meant to be used in the context of a 
#' clinical trial with a binary endpoint. Interest is in the response rate. 
#' The goal is to simulate response counts based on the number of patients at 
#' accrued at each interim look, and calculate the posterior predictive 
#' probability of success at the end of a trial, given the data available at 
#' each interim analysis. 
#'
#' @param prob vector of length two containing the probability of response in 
#' the standard of care and experimental arm c(p0, p1) for the two-sample case; 
#' vector of response probability for one-sample case
#' @param n matrix containing the total number of patients accrued so far at 
#' each interim look in the standard of care (column 1) and experimental 
#' (column 2) arms for two-sample case; vector of sample size accrued so far 
#' at each interim look for one-sample case
#' @param direction "greater" (default) if interest is in p(y1 > y0) and "less" 
#' if interest is in p(y1 < y0). 
#' @param p The target value to compare to in the one-sample case 
#' (i.e. the unacceptable response rate)
#' @param delta clinically meaningful difference between groups. 
#' Typically 0 (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param seed set the seed for the random number generation (default NULL)
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
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

sim_single_trial <- function(prob, n, direction = "greater", p = NULL, 
                             delta = 0, prior = c(0.5,0.5), S = 5000, 
                             seed = NULL, 
                             N, theta = 0.95){
  
  set.seed(seed)
  
  if(length(prob) == 2) {
    
  } else if(length(prob) == 1) {
    
    y1 <- stats::rbinom(n = 1, size = n[1], prob = prob) # responses at first look
    
    for(i in 2:length(n)) { # then add in new responses at each subsequent look
      
      y1 <- c(y1, y1[length(y1)] + stats::rbinom(n = 1, 
                                                 size = n[i] - n[i - 1], 
                                                 prob = prob)) 
    }
    
    pp <- purrr::map2_dbl(y1, n, 
                          ~calc_predictive(
                            y = .x, 
                            n = .y, 
                            p = p, delta = delta, seed = seed, N = N)
    )
  }
  
  return(cbind(y = y1, n = n, pp = pp))
  
}