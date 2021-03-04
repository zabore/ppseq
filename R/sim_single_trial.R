#' Simulate a single trial with posterior probability monitoring
#'
#' @description This function is meant to be used in the context of a 
#' clinical trial with a binary endpoint. The goal is to simulate event counts 
#' from the binomial distribution based on the number of patients 
#' accrued at each interim look, and calculate the posterior predictive 
#' probability of success (or futility) at the end of a trial, 
#' given the data available at each interim analysis. 
#'
#' @param prob vector of length two containing the probability of event in 
#' the standard of care and experimental arm c(p0, p1) for the two-sample case; 
#' integer of event probability for one-sample case
#' @param n matrix containing the total number of patients accrued so far at 
#' each interim look in the standard of care (column 1) and experimental 
#' (column 2) arms for two-sample case; vector of sample size accrued so far 
#' at each interim look for one-sample case
#' @param direction "greater" (default) if interest is in P(p1 > p0) and "less" 
#' if interest is in P(p1 < p0) for two-sample case. For one-sample case, 
#' "greater" if interest is in P(p > p0) and "less" if interest is in P(p < p0).
#' @param p0 The target value to compare to in the one-sample case
#' @param delta clinically meaningful difference between groups. 
#' Typically 0 (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param seed set the seed for the random number generation. Default is NULL.
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param theta The target posterior probability. e.g. Efficacy decision if 
#' P(p1 > p0) > theta for the two-sample case with greater direction. 
#' Default is 0.95.
#' 
#' @return Returns a matrix of number of responses, sample size, and posterior
#' predictive probability at each interim look
#'
#' @export

sim_single_trial <- function(prob, n, direction = "greater", p0 = NULL, 
                             delta = 0, prior = c(0.5,0.5), S = 5000, 
                             seed = NULL, 
                             N, theta = 0.95){

  if((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop("Exactly one of delta or p0 must be specified for the two-sample and 
         one-sample case, respectively")
  
  if(!direction %in% c("greater", "less")) 
    stop('direction must be either "greater" or "less"')
  
  if(length(prob) == 1 & is.null(p0))
    stop("p0 must be specified for the one-sample case")
  
  if(length(prob) == 2 & is.null(delta))
    stop("delta must be specified for the two-sample case")
  
  set.seed(seed)
  
  if(length(prob) == 2) {
    
    y0 <- stats::rbinom(n = 1, size = n[1, 1], prob = prob[1]) 
    y1 <- stats::rbinom(n = 1, size = n[1, 2], prob = prob[2]) 
    
    for(i in 2:nrow(n)) { 
      
      y0 <- c(y0, y0[length(y0)] + stats::rbinom(n = 1, 
                                                 size = n[i, 1] - n[i - 1, 1], 
                                                 prob = prob[1])) 
      y1 <- c(y1, y1[length(y1)] + stats::rbinom(n = 1, 
                                                 size = n[i, 2] - n[i - 1, 2], 
                                                 prob = prob[2])) 
    }
    
    pp <- purrr::pmap_dbl(list(y0, y1, n[, 1], n[, 2]), 
                          ~calc_predictive(
                            y = c(..1, ..2), 
                            n = c(..3, ..4), 
                            p0 = p0, delta = delta, prior = prior,
                            S = S, seed = seed, N = N, theta = theta)
                          )
    
    res <- cbind(y0, y1, n, pp); colnames(res)[3:4] <- c("n0", "n1")
    
  } else if(length(prob) == 1) {
    
    y1 <- stats::rbinom(n = 1, size = n[1], prob = prob) 
    
    for(i in 2:length(n)) { 
      
      y1 <- c(y1, y1[length(y1)] + stats::rbinom(n = 1, 
                                                 size = n[i] - n[i - 1], 
                                                 prob = prob)) 
    }
    
    pp <- purrr::map2_dbl(y1, n, 
                          ~calc_predictive(
                            y = .x, 
                            n = .y, 
                            p0 = p0, delta = delta, prior = prior,
                            S = S, seed = seed, N = N, theta = theta)
                          )
    
    res <- cbind(y = y1, n = n, pp = pp)
  }
  
  return(res)
  
}