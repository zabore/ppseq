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
#' @param n matrix containing the total number of patients accrued so far at
#' each interim look in the standard of care (column 1) and experimental
#' (column 2) arms for two-sample case; vector of sample size accrued so far
#' at each interim look for one-sample case. The last value should be equal to
#' the total sample size at the end of the trial.
#' If only a single look will be done
#' at the end of the trial, this can be a vector specifying the total sample
#' size c(N0, N1) for the two-sample case or an integer specifying the total
#' sample size N for the one-sample case
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param p0 The target value to compare to in the one-sample case. Set to NULL
#' for the two-sample case.
#' @param theta The target posterior probability. e.g. Efficacy decision if
#' P(p1 > p0) > theta for the two-sample case with greater direction.
#' @param ppp The target predictive probability. e.g. Stop the trial if the
#' predictive probability falls below this target.
#' @param direction "greater" (default) if interest is in P(p1 > p0) and "less"
#' if interest is in P(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in P(p > p0) and "less" if interest is in P(p < p0).
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for two-sample case. NULL for one-sample case (default). 
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#'
#' @return In the one-sample case, returns a tibble with n at each look, r at
#' each look, and ppp, the associated posterior predictive probability. 
#' Stop the trial at that look if the number of observed responses is <=r. 
#' At the end of the trial, the treatment is considered promising if the number 
#' of observed responses is >r. 
#' In the two-sample case, returns a tibble with n0 and n1, the number 
#' enrolled subjects in the control and experimental arms at each look, 
#' respectively, r0 and r1, the number of possible responses in the control and 
#' experimental arms at each look, respectively, and ppp, the associated 
#' posterior predictive probability. For a given value of r0, stop the trial if 
#' the number of observed responses in the experimental arm is <=r1. At the end
#' of the trial, the treatment is considered promising if the number of 
#' observed responses in the experimental arm is >r1 for a given r0.
#' Any NA value in either table represents an
#' interim look where there is no number of responses that would lead to 
#' stopping the trial.
#'
#' @examples
#' set.seed(123)
#' 
#' # One-sample case
#' calc_decision_rules(
#'   n = seq(5, 25, 5), 
#'   N = 25, 
#'   theta = 0.86, 
#'   ppp = 0.2, 
#'   p0 = 0.1, 
#'   S = 50
#'   )
#' 
#' # Two-sample case
#' calc_decision_rules(
#'   n = cbind(seq(5, 25, 5), seq(5, 25, 5)), 
#'   N = c(25, 25),
#'   theta = 0.86, 
#'   ppp = 0.2, 
#'   p0 = NULL, 
#'   direction = "greater", 
#'   delta = 0, 
#'   S = 50
#'   )
#'
#' @importFrom tibble tibble add_row
#' @importFrom dplyr arrange group_by slice
#' @export

calc_decision_rules <- function(n, N, theta, ppp, p0,
                                direction = "greater", delta = NULL, 
                                prior = c(0.5, 0.5), S = 5000) {
  if ((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop("Exactly one of delta or p0 must be specified for the ", 
         "two-sample and one-sample case, respectively")

  if (!direction %in% c("greater", "less"))
    stop('direction must be either "greater" or "less"')

  
  if(length(N) == 2) {
    
    res <- 
      tibble(
        n0 = rep(n[, 1], times = n[, 1] + 1),
        n1 = rep(n[, 2], times = n[, 1] + 1),
        r0 = unlist(lapply(n[, 1], function(x) seq(from = 0, to = x))),
        r1 = NA_integer_,
        ppp = NA_real_
      )
    
    
    ystart1 <- 0
    ns <- paste0(c(res[[1, "n0"]], res[[1, "n1"]]),collapse=",")
    for(i in 1:nrow(res)) {
      pred <- 0
      if( paste0(c(res[[i, "n0"]], res[[i, "n1"]]),collapse=",")!=ns ){
       ns <- paste0(c(res[[i, "n0"]], res[[i, "n1"]]),collapse=",")
       ystart1 <- 0
      }      
      for (j in ystart1:res[[i, "n1"]]) {
        pred <- calc_predictive(
          y = c(res[[i, "r0"]], j),
          n = c(res[[i, "n0"]], res[[i, "n1"]]),
          direction = direction,
          p0 = p0,
          delta = delta,
          prior = prior,
          S = S,
          N = N,
          theta = theta
        )
        if(pred > ppp) break else {
          ystart1 <- ifelse(j > 0, j - 1, j)
          
          res[i, "r1"] <- j
          res[i, "ppp"] <- pred
        }
      }
    }
    
  } else if(length(N) == 1) {
    
    res <- tibble(
      n = n,
      r = NA_integer_,
      ppp = NA_real_
    )
    
    ystart <- 0
    for (i in n) {
      pred <- 0
      for(j in ystart:i) {
        pred <- calc_predictive(
          y = j,
          n = i,
          direction = direction,
          p0 = p0,
          delta = delta,
          prior = prior,
          S = S,
          N = N,
          theta = theta
        )
        if(pred > ppp) break else{
          res[i == n, 2] <- j
          res[i == n, 3] <- pred
        }
      }
      ystart <- ifelse(j > 0, j - 1, j)
    }
    
  }
  
  # assign a custom class for S3 plotting methods
  class(res) <- c("calc_decision_rules", class(res))

  return(res)
}
