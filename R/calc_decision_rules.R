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
#' @return Returns a tibble with n at each look and r. Stop the trial at that
#' look if the number of observed responses is <=r. At the end of the trial,
#' the treatment is considered promising if the number of observed responses 
#' is >r.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#'
#' # One-sample case
#' calc_decision_rules(seq(5, 25, 5), p0 = 0.1, N = 25, theta = 0.86, ppp = 0.2)
#' 
#' # Two-sample case
#' calc_decision_rules(n = cbind(seq(5, 25, 5), seq(5, 25, 5)), N = c(25, 25),
#' theta = 0.86, ppp = 0.2, p0 = NULL, direction = "greater", delta = 0,
#' prior = c(0.5, 0.5), S = 5000)
#' }
#'
#' @importFrom tibble tibble add_row
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
    
    res <- tibble(
      n0 = as.numeric(),
      n1 = as.numeric(),
      r0 = as.numeric(),
      r1 = as.numeric()
    )
    
    for(i in 1:nrow(n)) {
      pred <- 0
      ytest0 <- ytest1 <- 0
      for (k in ytest0:n[i, 1]) {
        for (l in ytest1:n[i, 2]) {
          pred <- calc_predictive(
            y = c(k, l),
            n = c(n[i, 1], n[i, 2]),
            direction = direction,
            p0 = p0,
            delta = delta,
            prior = prior,
            S = S,
            N = N,
            theta = theta
          )
          if(pred > ppp) {
            ytest1 <- l
            
            res <- add_row(
              res,
              n0 = n[i, 1],
              n1 = n[i, 2],
              r0 = k,
              r1 = l
            )
            
            break
          }
        }
      }
    }
    
  } else if(length(N) == 1) {
    
    res <- tibble(
      n = n,
      r = rep(NA_integer_, length(n))
    )
  
    ytest <- 0
    for (i in n) {
      pred <- 0
      while (pred < ppp) {
        pred <- calc_predictive(
          y = ytest,
          n = i,
          direction = direction,
          p0 = p0,
          delta = delta,
          prior = prior,
          S = S,
          N = N,
          theta = theta
        )
        ytest <- ytest + 1
      }
      
      res[i == n, 2] <- ifelse(ytest == 0 & pred > ppp, NA, ytest - 1)
      ytest <- ytest - 1
    }
    
  }

  return(res)
}
